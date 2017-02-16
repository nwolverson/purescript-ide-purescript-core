module IdePurescript.PscIdeServer
  ( startServer
  , startServer'
  , stopServer
  , ServerStartResult(..)
  , ServerEff
  , Port
  , QuitCallback
  , ErrorLevel(..)
  , Notify(..)
  ) where

import Prelude
import PscIde.Server as S
import Control.Monad.Aff (runAff, Aff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (length, head)
import Data.Either (either)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (Pattern(Pattern), trim, split)
import Data.Traversable (traverse, traverse_)
import Global (readInt)
import IdePurescript.Exec (getPathVar, findBins)
import IdePurescript.PscIde (cwd) as PscIde
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, stderr, stdout)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Node.Stream (onDataString)
import PscIde (NET)
import PscIde.Server (Executable(Executable), getSavedPort, defaultServerArgs, savePort, pickFreshPort)

type Port = Int

data ServerStartResult =
    CorrectPath Port
  | WrongPath Port String
  | Started Port ChildProcess
  | Closed
  | StartError String

type ServerEff eff = (cp :: CHILD_PROCESS, process :: PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR, fs :: FS, err :: EXCEPTION, random :: RANDOM, buffer :: BUFFER | eff)

type QuitCallback eff = (Eff (err :: EXCEPTION, net :: NET, cp :: CHILD_PROCESS, fs :: FS | eff) Unit)

data ErrorLevel = Success | Info | Warning | Error
type Notify eff = ErrorLevel -> String -> Eff eff Unit

data Version = Version Int Int Int

parseVersion :: String -> Maybe Version
parseVersion s =
  case traverse fromNumber $ readInt 10 <$> split (Pattern ".") s of
    Just [a, b, c] -> Just $ Version a b c
    _ -> Nothing

instance eqVersion :: Eq Version where
  eq (Version a b c) (Version a' b' c') = a == a' && b == b' && c == c'

instance ordVersion :: Ord Version where
  compare (Version a b c) (Version a' b' c') = compare [a,b,c] [a',b',c']

instance showVersion :: Show Version where
  show (Version a b c) = show a <> "." <> show b <> "." <> show c


-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
-- | Will notify as to what is happening, choose to supply globs appropriately
startServer' :: forall eff eff'.
  String
  -> String
  -> Boolean
  -> Array String
  -> Notify (ServerEff eff)
  -> Notify (ServerEff eff)
  -> Aff (ServerEff eff) { quit :: QuitCallback eff', port :: Maybe Int }
startServer' path server addNpmBin glob cb logCb = do
  pathVar <- liftEff $ getPathVar addNpmBin path
  serverBins <- findBins pathVar server
  case head serverBins of
    Nothing -> do
      liftEff $ cb Info $ "Couldn't find psc-ide-server, check PATH. Looked for: "
        <> server <> " in PATH: " <> either id id pathVar
      pure { quit: pure unit, port: Nothing }
    Just (Executable bin version) -> do
      liftEff $ log $ "Resolved psc-ide-server paths (1st is used):"
      traverse_ (\(Executable x vv) -> do
        liftEff $ log $ x <> ": " <> fromMaybe "ERROR" vv) serverBins
      liftEff $ when (length serverBins > 1) $ cb Warning $ "Found multiple psc-ide-server executables; using " <> bin
      let glob' = if join (parseVersion <$> trim <$> version) >= Just (Version 0 9 2) then glob else []
      res <- startServer bin path glob'
      let noRes = { quit: pure unit, port: Nothing }
      liftEff $ case res of
        CorrectPath usedPort -> { quit: pure unit, port: Just usedPort } <$ cb Info ("Found existing psc-ide-server with correct path on port " <> show usedPort)
        WrongPath usedPort wrongPath -> do
          cb Error $ "Found existing psc-ide-server on port '" <> show usedPort <> "' with wrong path: '" <> wrongPath
            <> "'. Correct, kill or configure a different port, and restart."
          pure noRes
        Started usedPort cp -> do
          cb Success $ "Started psc-ide-server (port " <> show usedPort <> ")"
          wireOutput cp logCb
          pure
            { quit: void $ runAff (\_ -> pure unit) (\_ -> pure unit) $ stopServer usedPort path cp
            , port: Just usedPort
            }
        Closed -> noRes <$ cb Info "psc-ide-server exited with success code"
        StartError err -> noRes <$ (cb Error $ "Could not start psc-ide-server process. Check the configured port number is valid.\n" <> err)
  where
    wireOutput :: ChildProcess -> Notify (ServerEff eff) -> Eff (ServerEff eff) Unit
    wireOutput cp log = do
      onDataString (stderr cp) UTF8 (log Warning)
      onDataString (stdout cp) UTF8 (log Info)

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer :: forall eff. String -> String -> Array String -> Aff (ServerEff eff) ServerStartResult
startServer exe rootPath glob = do
  port <- liftEff $ getSavedPort rootPath
  case port of
    Just p -> do
      workingDir <- attempt $ PscIde.cwd p
      liftEff $ log $ "Found existing port from file: " <> show p <> (either (const "") (", cwd: " <> _) workingDir)
      either (const launchServer) (gotPath p) workingDir
    Nothing -> launchServer

  where
  launchServer = do
    newPort <- liftEff pickFreshPort
    liftEff $ do
      log $ "Starting psc-ide-server on port " <> show newPort <> " with cwd " <> rootPath
      savePort newPort rootPath
    r newPort <$> S.startServer (defaultServerArgs { exe = exe, cwd = Just rootPath, port = Just newPort, source = glob })
    where
      r newPort (S.Started cp) = Started newPort cp
      r _ (S.Closed) = Closed
      r _ (S.StartError s) = StartError s

  gotPath port workingDir =
    liftEff $ if workingDir == rootPath then
        do
          log $ "Found psc-ide-server on port " <> show port <> " with correct path: " <> workingDir
          pure $ CorrectPath port
      else
        do
          log $ "Found psc-ide-server on port " <> show port <> " with wrong path: " <> workingDir <> " instead of " <> rootPath
          pure $ WrongPath port workingDir

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: forall eff. Int -> String -> ChildProcess -> Aff (cp :: CHILD_PROCESS, net :: NET, fs :: FS, err :: EXCEPTION | eff) Unit
stopServer port rootPath cp = do
  oldPort <- liftEff $ S.getSavedPort rootPath
  liftEff $ when (oldPort == Just port) $ S.deleteSavedPort rootPath
  S.stopServer port
