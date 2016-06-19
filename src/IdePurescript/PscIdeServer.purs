module IdePurescript.PscIdeServer where

import Prelude
import PscIde.Server as S
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import IdePurescript.PscIde (cwd) as PscIde
import Node.ChildProcess (CHILD_PROCESS, ChildProcess)
import PscIde (NET)

data ServerStartResult =
    CorrectPath
  | WrongPath String
  | Started ChildProcess
  | Closed
  | StartError String

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer :: forall eff. String -> Int -> String -> Aff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR | eff) ServerStartResult
startServer exe port rootPath = do
  workingDir <- attempt $ PscIde.cwd port
  either (const launchServer) gotPath workingDir
  where

  launchServer = do
    liftEff $ log "Starting psc-ide-server"
    r <$> S.startServer exe port (Just rootPath)
    where
      r (S.Started cp) = Started cp
      r (S.Closed) = Closed
      r (S.StartError s) = StartError s

  gotPath workingDir =
    liftEff $ if workingDir == rootPath then
        do
          log $ "Found psc-ide-server with correct path: " <> workingDir
          pure CorrectPath
      else
        do
          log $ "Found psc-ide-server with wrong path: " <> workingDir <> " instead of " <> rootPath
          pure $ WrongPath workingDir

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: forall eff. Int -> ChildProcess -> Aff (cp :: CHILD_PROCESS, net :: NET | eff) Unit
stopServer port cp = S.stopServer port
