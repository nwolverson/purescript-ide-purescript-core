module IdePurescript.PscIdeServer where

import Prelude
import PscIde.Server as S
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (either)
import Data.Maybe (Maybe(Nothing, Just))
import IdePurescript.PscIde (cwd) as PscIde
import Node.ChildProcess (CHILD_PROCESS, ChildProcess)
import Node.FS (FS)
import PscIde (NET)
import PscIde.Server (pickFreshPort, savePort, getSavedPort)

type Port = Int

data ServerStartResult =
    CorrectPath Port
  | WrongPath Port String
  | Started Port ChildProcess
  | Closed
  | StartError String

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer :: forall eff. String -> String -> Aff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR, fs :: FS, err :: EXCEPTION, random :: RANDOM | eff) ServerStartResult
startServer exe rootPath = do
  port <- liftEff'' $ getSavedPort rootPath
  case port of
    Just p -> do
      workingDir <- attempt $ PscIde.cwd p
      liftEff'' $ log $ "Found existing port from file: " <> show p <> (either (const "") (", cwd: " <> _) workingDir)
      either (const launchServer) (gotPath p) workingDir
    Nothing -> launchServer

  where
  liftEff'' :: forall e a. Eff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR, fs :: FS, err :: EXCEPTION, random :: RANDOM | e) a
                        -> Aff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR, fs :: FS, err :: EXCEPTION, random :: RANDOM | e) a
  liftEff'' = liftEff

  launchServer = do
    newPort <- liftEff'' pickFreshPort
    liftEff $ do
      log $ "Starting psc-ide-server on port " <> show newPort <> " with cwd " <> rootPath
      savePort newPort rootPath
    r newPort <$> S.startServer exe newPort (Just rootPath)
    where
      r newPort (S.Started cp) = Started newPort cp
      r _ (S.Closed) = Closed
      r _ (S.StartError s) = StartError s

  gotPath port workingDir =
    liftEff $ if workingDir == rootPath then
        do
          log $ "Found psc-ide-server with correct path: " <> workingDir
          pure $ CorrectPath port
      else
        do
          log $ "Found psc-ide-server with wrong path: " <> workingDir <> " instead of " <> rootPath
          pure $ WrongPath port workingDir

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: forall eff. Int -> String -> ChildProcess -> Aff (cp :: CHILD_PROCESS, net :: NET, fs :: FS, err :: EXCEPTION | eff) Unit
stopServer port rootPath cp = do
  oldPort <- liftEff $ S.getSavedPort rootPath
  liftEff $ when (oldPort == Just port) $ S.deleteSavedPort rootPath
  S.stopServer port
