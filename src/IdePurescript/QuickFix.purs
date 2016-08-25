module IdePurescript.QuickFix where

-- | Get a title which explains what applying a compiler suggestion will do
getTitle :: String -> String
getTitle code = case code of
  "UnusedImport"                -> "Remove import"
  "RedundantEmptyHidingImport"  -> "Remove import"
  "DuplicateImport"             -> "Remove import"
  "RedundantUnqualifiedImport"  -> "Remove import"
  "DeprecatedQualifiedSyntax"   -> "Remove qualified keyword"
  "ImplicitImport"              -> "Make import explicit"
  "UnusedExplicitImport"        -> "Remove unused references"
  _                             -> "Apply Suggestion"

-- | Determine whether an error code represents an unknown token (unknown identifier or missing import)
isUnknownToken :: String -> Boolean
isUnknownToken code = case code of
  "UnknownValue" -> true
  "UnknownType" -> true
  "UnknownDataConstructor" -> true
  "UnknownTypeConstructor" -> true
  _ -> false
