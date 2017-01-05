module IdePurescript.Completion where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (filter)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, indexOf)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.PscIde (eitherToErr, getCompletion)
import IdePurescript.Regex (match', test')
import PscIde (NET, listAvailableModules)
import PscIde.Command (ModuleList(..), TypeInfo(..))

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  , mainModule :: Maybe String
  }

data SuggestionType = Module | Type | Function | Value

modulePart :: String
modulePart = """((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))"""

identPart :: String
identPart = "([a-zA-Z_][a-zA-Z0-9_']*)"

moduleRegex :: Either String Regex
moduleRegex = regex ("(?:^|[^A-Za-z_.])(?:" <> modulePart <> """\.)?""" <> identPart <> "?$") noFlags

explicitImportRegex :: Either String Regex
explicitImportRegex = regex ("""^import\s+""" <> modulePart <> """\s+\([^)]*?""" <> identPart <> "$") noFlags

getModuleSuggestions :: forall eff. Int -> String -> Aff (net :: NET | eff) (Array String)
getModuleSuggestions port prefix = do
  list <- eitherToErr $ listAvailableModules port
  pure $ case list of
    (ModuleList lst) -> filter (\m -> indexOf (Pattern prefix) m == Just 0) lst

data SuggestionResult =
  ModuleSuggestion { text :: String, suggestType :: SuggestionType, prefix :: String }
  | IdentSuggestion { mod :: String, identifier :: String, qualifier :: Maybe String, valueType :: String, suggestType :: SuggestionType, prefix :: String }

getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo
  } -> Aff (net :: NET | eff) (Array SuggestionResult)
getSuggestions port { line, moduleInfo: { modules, getQualifiedModule, mainModule } } =
  if moduleExplicit then
    case match' explicitImportRegex line of
      Just [ Just _, Just mod, Just token ] -> do
        completions <- getCompletion port token mainModule Nothing [ mod ] getQualifiedModule
        pure $ map (result (Just mod) token) completions
      _ -> pure []
  else
    case parsed of
      Just { mod, token } ->
        if moduleCompletion then do
          let prefix = getModuleName (fromMaybe "" mod) token
          completions <- getModuleSuggestions port prefix
          pure $ map (modResult prefix) completions
        else do
          completions <- getCompletion port token mainModule mod modules getQualifiedModule
          pure $ map (result mod token) completions
      Nothing -> pure []
    where
    getModuleName "" token  = token
    getModuleName mod token = mod <> "." <> token

    isImport = indexOf (Pattern "import") line == Just 0
    hasBracket = indexOf (Pattern "(") line /= Nothing
    moduleCompletion = isImport && not hasBracket
    moduleExplicit = isImport && hasBracket

    parsed = case match' moduleRegex line of
        Just [ Just _, mod, tok ] | mod /= Nothing || tok /= Nothing ->
          Just { mod, token: fromMaybe "" tok}
        _ -> Nothing

    modResult prefix moduleName = ModuleSuggestion { text: moduleName, suggestType: Module, prefix }
    result qualifier prefix (TypeInfo {type', identifier, module': mod}) =
      IdentSuggestion { mod, identifier, qualifier, suggestType, prefix, valueType: type' }
      where
        suggestType =
          if contains (Pattern "->") type' then Function
          else if test' (regex "^[A-Z]" noFlags) identifier then Type
          else Value
