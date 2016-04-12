module IdePurescript.Modules (
    Module
  , initialModulesState
  , State
  , getMainModule
  , getModulesForFile
  , getUnqualActiveModules
  , getAllActiveModules
  , getQualModule
  , findImportInsertPos
  , addModuleImport
  , addExplicitImport
  , ImportResult(..)
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array ((:), findLastIndex, filter, singleton, concatMap)
import Data.Either (either, Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (split)
import Data.String.Regex as R
import Data.Foldable (elem)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (sep)
import Prelude
import PscIde as P
import PscIde.Command as C

data Module = Implicit String | Explicit String (Array String) | Qualified String String

derive instance moduleEq :: Eq Module

getModuleName :: Module -> String
getModuleName (Qualified _ m) = m
getModuleName (Implicit m) = m
getModuleName (Explicit m _) = m

type State =
  { main :: Maybe String
  , modules :: Array Module
  , identifiers :: Array String
  }

type Path = String

getMainModule :: String -> Maybe String
getMainModule text =
  case R.match regex text of
    Just [_, Just m] -> Just m
    _ -> Nothing
  where
  regex = R.regex """module\s+([\w.]+)""" $ R.noFlags { multiline = true }

getModulesForFile :: forall eff. Path -> String -> Aff (net :: P.NET | eff) State
getModulesForFile file fullText = do
  imports <- P.listImports file
  let modules = either (const []) (\(C.ImportList l) -> map mod l) imports
      main = getMainModule fullText
      identifiers = concatMap idents modules
  pure { main, modules, identifiers }
  where
  mod (C.Import { moduleName, qualifier: Nothing, importType: C.Explicit identifiers }) =
    Explicit moduleName identifiers
  mod (C.Import { moduleName, qualifier: Nothing }) = Implicit moduleName
  mod (C.Import { moduleName, qualifier: Just qual }) = Qualified qual moduleName

  idents (Explicit _ x) = x
  idents _ = []

getUnqualActiveModules :: State -> Maybe String -> Array String
getUnqualActiveModules {modules, main} ident =
  map getModuleName $ maybe [] (singleton <<< Implicit) main ++ filter include modules
  where
  include (Qualified _ _) = false
  include (Explicit _ idents) = maybe false (_ `elem` idents) ident
  include (Implicit _) = true

getAllActiveModules  :: State -> Array String
getAllActiveModules {modules, main} =
  map getModuleName $ maybe [] (singleton <<< Implicit) main ++ modules

getQualModule :: String -> State -> Array String
getQualModule qualifier {modules} =
  map getModuleName $ filter (qual qualifier) modules
  where
  qual q (Qualified q' _) = q == q'
  qual _ _ = false

initialModulesState :: State
initialModulesState =  { main: Nothing, modules: [], identifiers: [] }

findImportInsertPos :: String -> Int
findImportInsertPos text =
  let regex = R.regex """^(module|import) [A-Z][^(]*($|\([^()]*\))""" R.noFlags
      lines = split "\n" text
      res = fromMaybe 0 $ findLastIndex (R.test regex) lines
  in res+1

foreign import tmpDir :: forall eff. Eff (fs :: FS | eff) String

data ImportResult = UpdatedImports String | AmbiguousImport (Array C.Completion) | FailedImport

withTempFile :: forall eff. String -> String -> (String -> Aff (net :: P.NET, fs :: FS | eff) (Either String C.ImportResult))
  -> Aff (net :: P.NET, fs :: FS | eff) ImportResult
withTempFile fileName text action = do
  dir <- liftEff tmpDir
  let name = R.replace (R.regex "[\\/\\\\]" (R.noFlags { global = true })) "-" fileName
      tmpFile = dir ++ sep ++ "ide-purescript." ++ name ++ ".purs"
  FS.writeTextFile UTF8 tmpFile text
  res <- action tmpFile
  answer <- case res of
    Right (C.SuccessFile _) -> UpdatedImports <$> FS.readTextFile UTF8 tmpFile
    Right (C.MultipleResults a) -> pure $ AmbiguousImport a
    _ -> pure FailedImport
  FS.unlink tmpFile
  pure answer

addModuleImport :: forall eff. State -> String -> String -> String
  -> Aff (net :: P.NET, fs :: FS | eff) (Maybe { state :: State, result :: String })
addModuleImport state fileName text moduleName =
  case shouldAdd of
    false -> pure Nothing
    true -> do
      res <- withTempFile fileName text addImport
      pure $ case res of
        UpdatedImports result -> Just { state, result }
        _ -> Nothing
  where
  addImport tmpFile = P.implicitImport tmpFile (Just tmpFile) [] moduleName
  shouldAdd =
    state.main /= Just moduleName && Implicit moduleName `elem` state.modules

addExplicitImport :: forall eff. State -> String -> String -> (Maybe String) -> String
  -> Aff (net :: P.NET, fs :: FS | eff) { state :: State, result :: ImportResult }
addExplicitImport state fileName text moduleName identifier =
  case shouldAdd of
    false -> pure { state, result: FailedImport }
    true -> do
      result <- withTempFile fileName text addImport
      let state' = case result of
            UpdatedImports _ -> state { identifiers = identifier : state.identifiers }
            _ -> state
      pure { result, state: state' }
  where
    addImport tmpFile = P.explicitImport tmpFile (Just tmpFile) filters identifier
    filters = case moduleName of
                Nothing -> []
                Just mn -> [C.ModuleFilter [mn]]
    isThisModule = case moduleName of
      Just _ -> moduleName == state.main
      _ -> false

    shouldAdd = not isThisModule
      && not (identifier `elem` state.identifiers)
      && maybe true (\mn -> not $ Implicit mn `elem` state.modules) moduleName
