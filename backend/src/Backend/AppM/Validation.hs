module Backend.AppM.Validation (validateM) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Servant.Server
import Validation

import Backend.AppM.Monad
import Backend.KeyError   (KeyErrors)

newtype ValidationErrors e = ValidationErrors (KeyErrors e)

instance ToJSON e => ToJSON (ValidationErrors e) where
    toJSON (ValidationErrors errors) = object [ "errors" .= errors ]

validateM :: (AppM m, ToJSON e) => Validation (KeyErrors e) a -> ServerError -> m a
validateM v err = validation merr pure v
  where body :: ToJSON e => KeyErrors e -> ByteString
        body = encode . ValidationErrors

        merr :: (ToJSON e, AppM m) => KeyErrors e -> m a
        merr lst = throwError $ err { errBody = body lst }
