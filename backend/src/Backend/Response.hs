module Backend.Response
    ( SuccessJson(..)
    , jsonError
    , internalError
    , unauthorizedError
    , notFoundError
    , errorFormatters
    ) where

import Data.Aeson
import Data.Text      (Text)
import Servant.Server

import qualified Data.Text as Text

data SuccessJson = SuccessJson

instance ToJSON SuccessJson where
    toJSON _ = object ["success" .= True]

newtype JsonError = JsonError Text

instance ToJSON JsonError where
    toJSON (JsonError msg) = object ["errors" .= [msg]]

jsonError :: ServerError -> Text -> ServerError
jsonError err msg = err { errBody = encode $ JsonError msg }

internalError :: ServerError
internalError = jsonError err500 "Internal server error"

unauthorizedError :: ServerError
unauthorizedError = jsonError err401 "Unauthorized"

notFoundError :: ServerError
notFoundError = jsonError err404 "Not found"

errorFormatters :: ErrorFormatters
errorFormatters = defaultErrorFormatters { bodyParserErrorFormatter  = errorFormater
                                         , urlParseErrorFormatter    = errorFormater
                                         , headerParseErrorFormatter = errorFormater
                                         , notFoundErrorFormatter    = notFoundFormatter
                                         }
  where errorFormater _ _ = jsonError err422 . Text.pack
        notFoundFormatter _ = notFoundError
