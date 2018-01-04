module IdePurescript.Pursuit where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Data.Argonaut.Decode ((.??))
import Data.Array (filter)
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, Affjax, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))

newtype PursuitSearchInfo = PursuitSearchInfo
  { typeOrValue :: Maybe String
  , mod :: Maybe String
  , typeText :: Maybe String
  , title :: Maybe String
  , typ :: String
  }

instance decodeJsonPursuitSearchInfo :: DecodeJson PursuitSearchInfo where
  decodeJson json =
    do
      obj <- decodeJson json
      typeOrValue <- obj .?? "typeOrValue"
      mod <- obj .?? "module"
      typeText <- pure $ hush $ obj .? "typeText"
      title <- obj .?? "title"
      typ <- obj .? "type"
      pure $ PursuitSearchInfo { typeOrValue, mod, typeText, title, typ }

newtype PursuitSearchResult = PursuitSearchResult
  { text :: String
  , markup :: String
  , url :: String
  , version :: String
  , package :: String
  , info :: PursuitSearchInfo
  }

instance decodeJsonPursuitSearchResult :: DecodeJson PursuitSearchResult where
  decodeJson json =
    do
      obj <- decodeJson json
      text <- obj .? "text"
      markup <- obj .? "markup"
      url <- obj .? "url"
      version <- obj .? "version"
      package <- obj .? "package"
      info <- obj .? "info"
      pure $ PursuitSearchResult { text, markup, url, version, package, info }

pursuitRequest :: forall e a. String -> Affjax e Json
pursuitRequest text = affjax $ defaultRequest
  { url = "https://pursuit.purescript.org/search?q=" <> text
  , headers = [ Accept applicationJSON ]
  }

pursuitSearchRequest :: forall eff. String -> Aff ( ajax :: AJAX | eff ) (Array PursuitSearchResult)
pursuitSearchRequest text = do
  res <- pursuitRequest text
  let decoded = decodeJson res.response
  pure $ either (pure []) id $ decoded

pursuitModuleSearchRequest :: forall eff. String -> Aff ( ajax :: AJAX | eff ) (Array PursuitSearchResult)
pursuitModuleSearchRequest text = do
  res <- pursuitRequest text
  let decoded = decodeJson res.response
      results = either (pure []) id $ decoded
  pure $ filter isModule results

  where
    isModule (PursuitSearchResult { info: PursuitSearchInfo { typ: "module" } }) = true
    isModule _ = false
