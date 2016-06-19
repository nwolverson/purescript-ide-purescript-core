module IdePurescript.Regex where

import Prelude
import Data.String.Regex as R
import Data.Either
import Data.Array
import Data.Maybe


replace' :: Either _ R.Regex -> String -> String -> String
replace' (Left _) _ s = s
replace' (Right r) t s = R.replace r t s

match' :: Either _ R.Regex -> String -> Maybe (Array (Maybe String))
match' (Left _) = const Nothing
match' (Right r) = R.match r

test' :: Either _ R.Regex -> String -> Boolean
test' (Left _) = const false
test' (Right r) = R.test r
