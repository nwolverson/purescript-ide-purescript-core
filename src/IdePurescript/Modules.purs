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

import Prelude
import Data.String.Regex as R
import Node.FS.Aff as FS
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array ((:), findLastIndex, filter, singleton, concatMap)
import Data.Either (either, Either(..))
import Data.Foldable (all, notElem, elem)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (split)
import IdePurescript.Regex (replace', match', test')
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.Path (sep)
import PscIde.Command (ImportType(..))

newtype Module = Module
  { moduleName :: String
  , importType :: C.ImportType
  , qualifier  :: Maybe String
  }

instance moduleEq :: Eq Module where
  eq (Module m1) (Module m2) =
    m1.moduleName == m2.moduleName &&
    m1.qualifier == m2.qualifier &&
    m1.importType `eqImportType` m2.importType

eqImportType :: C.ImportType -> C.ImportType -> Boolean
eqImportType Implicit Implicit = true
eqImportType (Explicit idents) (Explicit idents') = idents == idents'
eqImportType (Hiding idents) (Hiding idents') = idents == idents'
eqImportType _ _ = false

getModuleName :: Module -> String
getModuleName (Module { moduleName }) = moduleName

type State =
  { main :: Maybe String
  , modules :: Array Module
  , identifiers :: Array String
  }

type Path = String

getMainModule :: String -> Maybe String
getMainModule text =
  case match' regex text of
    Just [_, Just m] -> Just m
    _ -> Nothing
  where
  regex = R.regex """module\s+([\w.]+)""" $ R.noFlags { multiline = true }

getModulesForFile :: forall eff. Int -> Path -> String -> Aff (net :: P.NET | eff) State
getModulesForFile port file fullText = do
  imports <- P.listImports port file
  let modules = either (const []) (\(C.ImportList l) -> map mod l) imports
      main = getMainModule fullText
      identifiers = concatMap idents modules
  pure { main, modules, identifiers }
  where
  mod (C.Import imp) = Module imp
  idents (Module { importType: Explicit ids }) = ids
  idents _ = []

mkImplicit :: String -> Module
mkImplicit m = Module { qualifier: Nothing, importType: Implicit, moduleName: m }

getUnqualActiveModules :: State -> Maybe String -> Array String
getUnqualActiveModules {modules, main} ident =
  map getModuleName $ maybe [] (singleton <<< mkImplicit) main <> filter include modules
  where
  include (Module { qualifier: Just _ }) = false
  include (Module { importType: Explicit idents }) = maybe false (_ `elem` idents) ident
  include (Module { importType: Implicit }) = true
  include (Module { importType: Hiding idents }) =  maybe true (_ `notElem` idents) ident

getAllActiveModules  :: State -> Array String
getAllActiveModules {modules, main} =
  map getModuleName $ maybe [] (singleton <<< mkImplicit) main <> modules

getQualModule :: String -> State -> Array String
getQualModule qualifier {modules} =
  map getModuleName $ filter (qual qualifier) modules
  where
  qual q (Module { qualifier: Just q' }) = q == q'
  qual _ _ = false

initialModulesState :: State
initialModulesState =  { main: Nothing, modules: [], identifiers: [] }

findImportInsertPos :: String -> Int
findImportInsertPos text =
  let regex = R.regex """^(module|import) [A-Z][^(]*($|\([^()]*\))""" R.noFlags
      lines = split "\n" text
      res = fromMaybe 0 $ findLastIndex (test' regex) lines
  in res+1

foreign import tmpDir :: forall eff. Eff (fs :: FS | eff) String

data ImportResult = UpdatedImports String | AmbiguousImport (Array C.TypeInfo) | FailedImport

withTempFile :: forall eff. String -> String -> (String -> Aff (net :: P.NET, fs :: FS | eff) (Either String C.ImportResult))
  -> Aff (net :: P.NET, fs :: FS | eff) ImportResult
withTempFile fileName text action = do
  dir <- liftEff tmpDir
  let name = replace' (R.regex "[\\/\\\\]" (R.noFlags { global = true })) "-" fileName
      tmpFile = dir <> sep <> "ide-purescript." <> name <> ".purs"
  FS.writeTextFile UTF8 tmpFile text
  res <- action tmpFile
  answer <- case res of
    Right (C.SuccessFile _) -> UpdatedImports <$> FS.readTextFile UTF8 tmpFile
    Right (C.MultipleResults a) -> pure $ AmbiguousImport a
    _ -> pure FailedImport
  FS.unlink tmpFile
  pure answer

addModuleImport :: forall eff. State -> Int -> String -> String -> String
  -> Aff (net :: P.NET, fs :: FS | eff) (Maybe { state :: State, result :: String })
addModuleImport state port fileName text moduleName =
  case shouldAdd of
    false -> pure Nothing
    true -> do
      res <- withTempFile fileName text addImport
      pure $ case res of
        UpdatedImports result -> Just { state, result }
        _ -> Nothing
  where
  addImport tmpFile = P.implicitImport port tmpFile (Just tmpFile) [] moduleName
  shouldAdd =
    state.main /= Just moduleName && (mkImplicit moduleName `notElem` state.modules)

addExplicitImport :: forall eff. State -> Int -> String -> String -> (Maybe String) -> String
  -> Aff (net :: P.NET, fs :: FS | eff) { state :: State, result :: ImportResult }
addExplicitImport state port fileName text moduleName identifier =
  case shouldAdd of
    false -> pure { state, result: FailedImport }
    true -> do
      result <- withTempFile fileName text addImport
      let state' = case result of
            UpdatedImports _ -> state { identifiers = identifier : state.identifiers }
            _ -> state
      pure { result, state: state' }
  where
    addImport tmpFile = P.explicitImport port tmpFile (Just tmpFile) filters identifier
    filters = case moduleName of
                Nothing -> []
                Just mn -> [C.ModuleFilter [mn]]
    isThisModule = case moduleName of
      Just _ -> moduleName == state.main
      _ -> false

    shouldAdd = not isThisModule
      && not (identifier `elem` state.identifiers)
      && maybe true (\mn -> all (shouldAddMatch mn) state.modules) moduleName

    shouldAddMatch mn (Module { moduleName: moduleName', qualifier: Nothing, importType: Implicit })
      | moduleName' == mn = false
    shouldAddMatch mn (Module { moduleName: moduleName', qualifier: Nothing, importType: Hiding idents })
      | moduleName' == mn = identifier `elem` idents
    shouldAddMatch _ _ = true
