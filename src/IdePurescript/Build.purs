module IdePurescript.Build where

import Prelude
import Node.ChildProcess as CP
import Node.Stream as S
import PscIde as P
import PscIde.Command as PC
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error, catchException)
import Control.Monad.Eff.Ref (readRef, REF, modifyRef, newRef)
import Control.Monad.Error.Class (throwError)
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.String (split, indexOf)
import IdePurescript.PscErrors (RebuildResult(RebuildResult), PscResult, parsePscOutput)
import Node.Encoding (Encoding(UTF8))
import PscIde (NET)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

type BuildOptions =
  { command :: Command
  , directory :: String
  }

data Command = Command String (Array String)

newtype BuildError = BuildError {}
type BuildResult =
  { errors :: PscResult
  , success :: Boolean
  }

addExceptionEffect :: forall eff a. Eff eff a -> Eff (err :: EXCEPTION | eff) a
addExceptionEffect = unsafeInterleaveEff

build :: forall eff. BuildOptions -> Aff (cp :: CP.CHILD_PROCESS, console :: CONSOLE, ref :: REF | eff) BuildResult
build { command: Command cmd args, directory } = makeAff $ \err succ -> do
  cp <- CP.spawn cmd args (CP.defaultSpawnOptions { cwd = Just directory })
  CP.onError cp (err <<< CP.toStandardError)
  let stderr = CP.stderr cp
  result <- newRef ""
  let res :: String -> Eff (console :: CONSOLE, cp :: CP.CHILD_PROCESS, err :: EXCEPTION, ref :: REF | eff) Unit
      res s = do
        modifyRef result (\acc -> acc<>s)

  catchException err $ S.onDataString stderr UTF8 res
  CP.onClose cp (\exit -> case exit of
    CP.Normally n | n == 0 || n == 1 -> do
      pscOutput <- readRef result
      let lines = split "\n" pscOutput
          json = find (\s -> indexOf "{\"" s == Just 0) lines
      case parsePscOutput <$> json of
        Just (Left e) -> err $ error e
        Just (Right r) -> succ { errors: r, success: n == 0 }
        Nothing -> err $ error "Didn't find JSON output"
    _ -> err $ error "Process exited abnormally")


rebuild :: forall eff. Int -> String -> Aff (net :: NET | eff) BuildResult
rebuild port file = do
  res <- rebuild' port file
  either
    (throwError <<< error)
    (pure <<< onResult)
    res
  where
  onResult :: Either RebuildResult RebuildResult -> BuildResult
  onResult =
    either (\(RebuildResult errors)   -> { errors: { errors, warnings: [] }, success: false })
           (\(RebuildResult warnings) -> { errors: { errors: [], warnings }, success: true  })

  rebuild' :: Int -> String -> P.CmdR RebuildResult RebuildResult
  rebuild' port file = P.sendCommandR port (PC.RebuildCmd file)
