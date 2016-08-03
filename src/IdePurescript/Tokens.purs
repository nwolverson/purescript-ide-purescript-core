module IdePurescript.Tokens where

import Prelude (const, (<>), (-), (+))

import Data.Maybe (Maybe(..))
import Data.Either
import Data.String (length, take, drop)

import Data.String.Regex (match, noFlags, regex)

type WordRange = { left :: Int, right :: Int }

identifierAtPoint :: String -> Int -> Maybe { word :: String, range :: WordRange, qualifier :: Maybe String }
identifierAtPoint line column =
  let beforeRegex = regex "[a-zA-Z_0-9']*$" noFlags
      afterRegex = regex "^[a-zA-Z_0-9']*" noFlags
      moduleRegex = regex """(?:^|[^A-Za-z_.])(?:((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.)$""" noFlags
      textBefore = take column line
      textAfter = drop column line
      wordRange left right = { left: column - left, right: column + right }
      match' r t = either (const Nothing) (\r' -> match r' t) r
  in
  case match' beforeRegex textBefore, match' afterRegex textAfter of
    Just [Just s], Just [Just s'] ->
        let qualifier = case match' moduleRegex (take (length textBefore - length s) textBefore) of
                            Just [ _, mm ] -> mm
                            _ -> Nothing
        in
          Just { word : s<>s', range : wordRange (length s) (length s'), qualifier }
    _, _ -> Nothing
