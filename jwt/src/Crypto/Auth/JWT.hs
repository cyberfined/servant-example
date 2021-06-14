{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Crypto.Auth.JWT
    ( JWT
    , Token(..)
    , Secret(..)
    , JWTError(..)
    , initJwt
    , token
    , payload
    , tokenPure
    , payloadPure
    ) where

import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC        (HMAC, hmac)
import Control.Monad.Time
import Data.Aeson             (ToJSON(..), FromJSON(..))
import Data.ByteArray         (ByteArrayAccess, convert)
import Data.ByteString        (ByteString, split)
import Data.ByteString.Lazy   (toStrict)
import Data.Char              (ord)
import Data.Time.Clock        (UTCTime, NominalDiffTime, addUTCTime)
import GHC.Generics

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Base64 as Base64

data JWT a = JWT
    { jwtAlgorithm :: !a
    , jwtSecret    :: !Secret
    , jwtDuration  :: !NominalDiffTime
    }

newtype Token = Token { unToken :: ByteString } deriving Eq

newtype Secret = Secret ByteString deriving newtype ByteArrayAccess

data Header = Header
    { hType       :: !String
    , hAlgorithm  :: !String
    , hExpiration :: !UTCTime
    } deriving (Generic, ToJSON, FromJSON)

data JWTError = WrongSignature
              | WrongEncoding
              | WrongTokenType
              | WrongAlgorithm
              | TokenExpired
              deriving Eq

instance Show JWTError where
    show = \case
        WrongSignature -> "wrong JWT signature"
        WrongEncoding  -> "corrupted JWT json data"
        WrongTokenType -> "wrong JWT token type"
        WrongAlgorithm -> "wrong JWT algorithm"
        TokenExpired   -> "JWT token has expired"

initJwt :: (Show a, HashAlgorithm a) => a -> Secret -> NominalDiffTime -> JWT a
initJwt = JWT

token :: (MonadTime m, Show a, HashAlgorithm a, ToJSON j)
      => JWT a    -- jwt info
      -> j        -- payload
      -> m Token  -- resulted token
token jwt plData = tokenPure jwt plData <$> currentTime

tokenPure :: (Show a, HashAlgorithm a, ToJSON j) => JWT a -> j -> UTCTime -> Token
tokenPure jwt plData time = Token tok
  where hdr = Header { hType       = tokenType
                     , hAlgorithm  = show $ jwtAlgorithm jwt
                     , hExpiration = addUTCTime (jwtDuration jwt) time
                     }
        hdrEnc = Base64.encode $ toStrict $ Aeson.encode hdr
        plEnc = Base64.encode $ toStrict $ Aeson.encode plData
        tok = hdrEnc <> "." <> plEnc <> "." <> signature jwt hdrEnc plEnc

payload :: (MonadTime m, Show a, HashAlgorithm a, FromJSON j)
        => JWT a                 -- jwt info
        -> Token                 -- token
        -> m (Either JWTError j) -- error or payload
payload jwt tok = do
    curTime <- currentTime
    pure $ payloadPure jwt tok curTime

payloadPure :: (Show a, HashAlgorithm a, FromJSON j)
            => JWT a
            -> Token
            -> UTCTime
            -> Either JWTError j
payloadPure jwt tok curTime = do
    (hdr, plData) <- decodePayload jwt tok
    case checkHeader jwt hdr curTime of
        Just err -> Left err
        Nothing  -> pure plData

decodePayload :: (Show a, HashAlgorithm a, FromJSON j)
              => JWT a
              -> Token
              -> Either JWTError (Header, j)
decodePayload jwt tok
  | length parts /= 3                  = Left WrongEncoding
  | signature jwt hdrEnc plEnc /= sign = Left WrongSignature
  | otherwise                          = maybe (Left WrongEncoding) Right maybeRes
  where parts = split (fromIntegral $ ord '.') $ unToken tok
        [hdrEnc, plEnc, sign] = parts
        maybeRes = case eitherRes of
            Left _ -> Nothing
            Right (hdrJson, plJson) -> (,)
                                    <$> Aeson.decodeStrict' hdrJson
                                    <*> Aeson.decodeStrict' plJson
        eitherRes = (,) <$> Base64.decode hdrEnc <*> Base64.decode plEnc

checkHeader :: (Show a, HashAlgorithm a) => JWT a -> Header -> UTCTime -> Maybe JWTError
checkHeader jwt hdr curTime
  | hType hdr /= tokenType                    = Just WrongTokenType
  | hAlgorithm hdr /= show (jwtAlgorithm jwt) = Just WrongAlgorithm
  | hExpiration hdr <= curTime                = Just TokenExpired
  | otherwise                                 = Nothing

signature :: forall a. (Show a, HashAlgorithm a)
          => JWT a
          -> ByteString
          -> ByteString
          -> ByteString
signature jwt hdr pl = Base64.encode $
    convert (hmac (jwtSecret jwt) (hdr <> pl) :: HMAC a)

tokenType :: String
tokenType = "JWT"
