module IdePurescript.PscErrors where

import Prelude
import Control.Alt ((<|>))
import Control.Bind ((<=<))
import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (JObject, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (singleton)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (traverse)

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

newtype RebuildResult = RebuildResult (Array PscError)

type PscResult =
  { warnings :: Array PscError
  , errors :: Array PscError
  }

newtype PscError = PscError
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  , errorLink :: String
  , suggestion :: Maybe String
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

instance decodeRebuildResult :: DecodeJson RebuildResult where
  decodeJson json = RebuildResult <$> (decodeJson json <|> (singleton <$> decodeJson json))

instance decodeJsonPscError :: DecodeJson PscError where
  decodeJson json = decodeJson json >>= parsePscError

parsePscOutput :: String -> Either String PscResult
parsePscOutput = maybe (Left "not object") parsePscResult <<< toObject <=< jsonParser

parsePscResult :: JObject -> Either String PscResult
parsePscResult obj =
  { warnings: _
  , errors: _
  } <$> (obj .? "warnings" >>= traverse parsePscError)
    <*> (obj .? "errors" >>= traverse parsePscError)

parsePscError :: JObject -> Either String PscError
parsePscError obj = PscError <$> (
  { moduleName: _
  , errorCode: _
  , message: _
  , filename: _
  , position: _
  , errorLink: _
  , suggestion: _
  } <$> obj .? "moduleName"
    <*> obj .? "errorCode"
    <*> obj .? "message"
    <*> obj .? "filename"
    <*> (obj .? "position" >>= parsePosition)
    <*> obj .? "errorLink"
    <*> (obj .? "suggestion" >>= parseSuggestion))

parsePosition :: Maybe JObject -> Either String (Maybe Position)
parsePosition =
  maybe (pure Nothing) \obj -> map Just $
    { startLine: _
    , startColumn: _
    , endLine: _
    , endColumn: _
    } <$> obj .? "startLine"
      <*> obj .? "startColumn"
      <*> obj .? "endLine"
      <*> obj .? "endColumn"

parseSuggestion :: Maybe JObject -> Either String (Maybe String)
parseSuggestion =
  maybe (pure Nothing) \obj -> obj .? "replacement"
