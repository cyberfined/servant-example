{-# OPTIONS_GHC -fno-warn-orphans #-}

module Factories.User
    ( module Backend.User.Model
    , module Backend.User.Statements
    ) where

import Crypto.Hash
import Data.ByteArray         (convert)
import Data.ByteString        (ByteString)
import Data.Default.Class
import System.Random.Stateful

import Backend.User.Model
import Backend.User.Statements

import qualified Data.Text as Text

instance Default User where
    def = User
        { userId           = def
        , userEmail        = Email "user@example.com"
        , userLogin        = Login "user"
        , userPasswordHash = passwordHash "password"
        , userImage        = Just $ URL "https://example.com/image0.png"
        , userBio          = Just $ Bio "Born. Lived. Died"
        , userCreatedAt    = def
        , userToken        = def
        }

instance Uniform User where
    uniformM g =  User
              <$> pure def
              <*> randEmail
              <*> randLogin
              <*> pure (passwordHash "password")
              <*> randImage
              <*> pure (Just $ Bio "Born. Lived. Died")
              <*> pure def
              <*> pure def
      where randEmail = randPositive >>= pure . genEmail
            randLogin = randPositive >>= pure . genLogin
            randImage = randPositive >>= pure . genImage
            randPositive = uniformRM (0, maxBound) g

            genEmail :: Int -> Email
            genEmail i = Email $ "user" <> Text.pack (show i) <> "@example.com"

            genLogin :: Int -> Login
            genLogin i = Login $ "user" <> Text.pack (show i)

            genImage :: Int -> Maybe URL
            genImage i =  Just $ URL $ "https://example.com/image"
                       <> Text.pack (show i) <> ".png"

passwordHash :: ByteString -> PasswordHash
passwordHash = PasswordHash . convert . hashWith SHA256
