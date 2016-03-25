module IdePurescript.Modules (
    Module
  , initialModulesState
  , State
  , getMainModule
  , getModulesForFile
  , getUnqualActiveModules
  , getQualModule
  , findImportInsertPos
  , addModuleImport
  , addExplicitImport
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Array (filter, singleton, findLastIndex)
import Control.Monad.Aff (Aff)
import Data.Either (either, Either(..))
import Data.String (split)
import Data.String.Regex as R

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (sep)
import Node.Encoding (Encoding(UTF8))

import PscIde as P
import PscIde.Command as C

data Module = Unqualified String | Qualified String String

getModuleName :: Module -> String
getModuleName (Qualified _ m) = m
getModuleName (Unqualified m) = m

type State =
  { main :: Maybe String
  , modules :: Array Module
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
  let main = getMainModule fullText
  pure { main, modules }
  where
  mod (C.Import { moduleName, qualifier: Nothing }) = Unqualified moduleName
  mod (C.Import { moduleName, qualifier: Just qual }) = Qualified qual moduleName

getUnqualActiveModules :: State -> Array String
getUnqualActiveModules {modules, main} =
  map getModuleName $ maybe [] (singleton <<< Unqualified) main ++ modules

getQualModule :: String -> State -> Array String
getQualModule qualifier {modules} =
  map getModuleName $ filter (qual qualifier) modules
  where
  qual q (Qualified q' _) = q == q'
  qual _ _ = false

initialModulesState :: State
initialModulesState =  { main: Nothing, modules: [] }

findImportInsertPos :: String -> Int
findImportInsertPos text =
  let regex = R.regex """^(module|import) [A-Z][^(]*($|\([^()]*\))""" R.noFlags
      lines = split "\n" text
      res = fromMaybe 0 $ findLastIndex (R.test regex) lines
  in res+1

foreign import tmpDir :: forall eff. Eff (fs :: FS | eff) String

withTempFile :: forall eff. String -> String -> (String -> Aff (net :: P.NET, fs :: FS | eff) (Either String C.ImportResult))
  -> Aff (net :: P.NET, fs :: FS | eff) (Maybe String)
withTempFile fileName text action = do
  dir <- liftEff tmpDir
  let name = R.replace (R.regex "[\\/\\\\]" (R.noFlags { global = true })) "-" fileName
  let tmpFile = dir ++ sep ++ "ide-purescript." ++ name ++ ".purs"
  FS.writeTextFile UTF8 tmpFile text
  res <- action tmpFile
  case res of
    Right (C.SuccessFile _) -> Just <$> FS.readTextFile UTF8 tmpFile
    _-> pure Nothing

addModuleImport :: forall eff. String -> String -> String -> Aff (net :: P.NET, fs :: FS | eff) (Maybe String)
addModuleImport fileName text moduleName =
  withTempFile fileName text \tmpFile -> P.implicitImport tmpFile (Just tmpFile) [] moduleName

addExplicitImport :: forall eff. String -> String -> (Maybe String) -> String -> Aff (net :: P.NET, fs :: FS | eff) (Maybe String)
addExplicitImport fileName text moduleName identifier =
  withTempFile fileName text \tmpFile -> P.explicitImport tmpFile (Just tmpFile) filters identifier
  where
    filters = case moduleName of
                Nothing -> []
                Just mn -> [C.ModuleFilter [mn]]
