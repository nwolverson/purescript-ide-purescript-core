module IdePurescript.Modules (
    Module
  , initialModulesState
  , State
  , getMainModule
  , getModulesForFile
  , getModulesForFileTemp
  , getUnqualActiveModules
  , getAllActiveModules
  , getQualModule
  , findImportInsertPos
  , addModuleImport
  , addExplicitImport
  , ImportResult(..)
  ) where

import Prelude
import Node.FS.Aff as FS
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (findLastIndex, filter, singleton, concatMap, (:))
import Data.Either (either, Either(..))
import Data.Foldable (all, notElem, elem)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(Pattern), split)
import Data.String.Regex (regex) as R
import Data.String.Regex.Flags (global, noFlags, multiline) as R
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

derive instance moduleNewtype :: Newtype Module _

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
  regex = R.regex """module\s+([\w.]+)""" $ R.multiline

getModulesForFile :: forall eff. Int -> Path -> String -> Aff (net :: P.NET | eff) State
getModulesForFile port file fullText = do
  C.ImportList { moduleName, imports } <- either (const default) id <$> P.listImports port file
  let modules = map mod imports
      main = maybe (getMainModule fullText) Just moduleName
      identifiers = concatMap idents modules
  pure { main, modules, identifiers }
  where
  default = C.ImportList { moduleName: Nothing, imports: [] }
  mod (C.Import imp) = Module imp
  idents (Module { importType: Explicit ids }) = ids
  idents _ = []

getModulesForFileTemp :: forall eff. Int -> Path -> String -> Aff (net :: P.NET, fs :: FS | eff) State
getModulesForFileTemp port file fullText = do
  tmpFile <- makeTempFile file fullText
  res <- getModulesForFile port tmpFile fullText
  _ <- attempt $ FS.unlink tmpFile
  pure res

mkImplicit :: String -> Module
mkImplicit m = Module { qualifier: Nothing, importType: Implicit, moduleName: m }

getUnqualActiveModules :: State -> Maybe String -> Array String
getUnqualActiveModules {modules, main} ident =
  map getModuleName $ maybe [] (singleton <<< mkImplicit) main <> filter include modules
  where
  include (Module { qualifier: Just _ }) = false
  include (Module { importType: Explicit idents }) = maybe false (\x -> x `elem` idents || ("(" <> x <> ")") `elem` idents) ident
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
      lines = split (Pattern "\n") text
      res = fromMaybe 0 $ findLastIndex (test' regex) lines
  in res+1

foreign import tmpDir :: forall eff. Eff (fs :: FS | eff) String

data ImportResult = UpdatedImports String | AmbiguousImport (Array C.TypeInfo) | FailedImport

makeTempFile :: forall eff. Path -> String -> Aff (fs :: FS | eff) Path
makeTempFile fileName text = do
  dir <- liftEff tmpDir
  let name = replace' (R.regex "[\\/\\\\:]" R.global) "-" fileName
      tmpFile = dir <> sep <> "ide-purescript-" <> name
  FS.writeTextFile UTF8 tmpFile text
  pure tmpFile

withTempFile :: forall eff. String -> String -> (String -> Aff (net :: P.NET, fs :: FS | eff) (Either String C.ImportResult))
  -> Aff (net :: P.NET, fs :: FS | eff) ImportResult
withTempFile fileName text action = do
  tmpFile <- makeTempFile fileName text
  res <- action tmpFile
  answer <- case res of
    Right (C.SuccessFile _) -> UpdatedImports <$> FS.readTextFile UTF8 tmpFile
    Right (C.MultipleResults a) -> pure $ AmbiguousImport a
    _ -> pure FailedImport
  _ <- attempt $ FS.unlink tmpFile
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
