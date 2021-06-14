{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad.Reader
import Control.Monad.Time
import Crypto.Hash.Algorithms (SHA256(..))
import Data.Aeson             (ToJSON(..), FromJSON(..))
import Data.Time.Clock
import Data.Time.Format       (parseTimeOrError, defaultTimeLocale)
import GHC.Generics
import Test.HUnit

import Crypto.Auth.JWT

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestLabel "JWT test" $ TestList
    [ TestLabel "with valid token" $ TestList
        [ TestLabel "it returns a token with a valid payload" $
            TestCase validPayloadTest
        ]
    , TestLabel "with invalid token" $ TestList
        [ TestLabel "it returns an error if there is lesser than 3 token parts" $
            TestCase lesserTokenPartsErrorTest
        , TestLabel "it returns an error if there is greater than 3 token parts" $
            TestCase greaterTokenPartsErrorTest
        , TestLabel "it returns an error if json is corrupted" $
            TestCase corruptedJsonErrorTest
        , TestLabel "it returns an error if token has expired" $
            TestCase expiredTokenErrorTest
        , TestLabel "it returns an error if signature is wrong" $
            TestCase invalidSignatureErrorTest
        ]
    ]

data Payload = Payload
    { firstData  :: !String
    , secondData :: !Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SomeData = SomeData
    { someData :: !Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

jwtAlgorithm :: SHA256
jwtAlgorithm = SHA256

jwtSecret :: Secret
jwtSecret = Secret "secret"

jwtDuration :: NominalDiffTime
jwtDuration = nominalDay

jwt :: JWT SHA256
jwt = initJwt jwtAlgorithm jwtSecret jwtDuration

validPayloadTest :: Assertion
validPayloadTest = do
    tok <- token jwt expPayload
    res <- payload jwt tok
    case res of
        Left err         -> assertString $ "Unexpected JWT decoding error: " ++ show err
        Right actPayload -> assertEqual "Payload decoding error" expPayload actPayload
  where expPayload = Payload "first" 2

lesserTokenPartsErrorTest :: Assertion
lesserTokenPartsErrorTest = do
    res <- payload jwt (Token "1.2") :: IO (Either JWTError Payload)
    case res of
        Left err  -> assertEqual "Wrong error result" WrongEncoding err
        Right pay -> unexpectedSuccess WrongEncoding pay

greaterTokenPartsErrorTest :: Assertion
greaterTokenPartsErrorTest = do
    res <- payload jwt (Token "1.2.3.4") :: IO (Either JWTError Payload)
    case res of
        Left err  -> assertEqual "Wrong error result" WrongEncoding err
        Right pay -> unexpectedSuccess WrongEncoding pay

corruptedJsonErrorTest :: Assertion
corruptedJsonErrorTest = do
    tok <- token jwt actPayload
    res <- (payload jwt tok :: IO (Either JWTError Payload))
    case res of
        Left err  -> assertEqual "Wrong error result" WrongEncoding err
        Right pay -> unexpectedSuccess WrongEncoding pay
  where actPayload = SomeData 1

newtype TimeFreeze a = TimeFreeze { unTimeFreeze :: Reader UTCTime a }
    deriving newtype (Functor, Applicative, Monad, MonadReader UTCTime)

instance MonadTime TimeFreeze where
    currentTime = ask

expiredTokenErrorTest :: Assertion
expiredTokenErrorTest = case runReader (unTimeFreeze mRes) now of
    Left err     -> assertEqual "Wrong error result" TokenExpired err
    Right resPay -> unexpectedSuccess TokenExpired resPay
  where pay = Payload "first" 2
        now = parseTime "2021-10-30 18:46:00"
        afterOneDay = parseTime "2021-10-31 18:46:00"
        parseTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        mRes :: TimeFreeze (Either JWTError Payload)
        mRes = do
            tok <- token jwt pay
            local (const afterOneDay) $ payload jwt tok

invalidSignatureErrorTest :: Assertion
invalidSignatureErrorTest = do
    tok <- token jwt2 pay
    res <- (payload jwt1 tok :: IO (Either JWTError Payload))
    case res of
        Left err     -> assertEqual "Wrong error result" WrongSignature err
        Right resPay -> unexpectedSuccess WrongSignature resPay
  where jwt1 = jwt
        jwt2 = initJwt jwtAlgorithm (Secret "1234") jwtDuration
        pay = Payload "first" 2

unexpectedSuccess :: Show a => JWTError -> a -> Assertion
unexpectedSuccess err pay =
    assertString $ "Expected " ++ show err ++
                " error, but payload " ++ show pay ++ " was returned"
