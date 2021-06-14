module Backend.User.Model
    ( UserF(..)
    , User
    , Login(..)
    , Email(..)
    , Password(..)
    , PasswordHash(..)
    , URL(..)
    , Bio(..)
    , Token(..)
    ) where

import Data.Aeson                 (ToJSON(..), (.=), object)
import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Database.Hasqul
import Database.Hasqul.Updater

import Backend.Common

type User = UserF Create

data UserF f = User
    { userId           :: !(Column f (Key User))
    , userEmail        :: !(Column f Email)
    , userLogin        :: !(Column f Login)
    , userPasswordHash :: !(Column f PasswordHash)
    , userImage        :: !(Column f (Maybe URL))
    , userBio          :: !(Column f (Maybe Bio))
    , userCreatedAt    :: !(Column f Timestamp)
    , userToken        :: !(Column f Token)
    } deriving Generic

deriving instance Show User
deriving via Encoder '[IgnoreField "userToken", NoEncodeField "userCreatedAt"] User
    instance Codec User
deriving instance Show (UserF Update)
deriving via Updater '[IgnoreField "userToken"] (UserF Update)
    instance Updatable (UserF Update)

instance ToJSON User where
    toJSON User{..} = object ["user" .= user]
      where user = object [ "email"    .= userEmail
                          , "username" .= userLogin
                          , "bio"      .= userBio
                          , "image"    .= userImage
                          , "token"    .= userToken
                          ]

newtype Login = Login { getLogin :: Text }
    deriving newtype (Eq, Show, ToJSON, Valuable)
    deriving Codec via ValueCodec Login

newtype Email = Email { getEmail :: Text }
    deriving newtype (Eq, Show, ToJSON, Valuable)
    deriving Codec via ValueCodec Email

newtype Password = Password { getPassword :: Text } deriving newtype Eq

newtype PasswordHash = PasswordHash { getPasswordHash :: ByteString }
    deriving newtype (Eq, Show, Valuable)
    deriving Codec via ValueCodec PasswordHash

newtype URL = URL { getURL :: Text }
    deriving newtype (Eq, Show, ToJSON, Valuable)
    deriving Codec via ValueCodec URL

newtype Bio = Bio { getBio :: Text }
    deriving newtype (Eq, Show, ToJSON, Valuable)
    deriving Codec via ValueCodec Bio

newtype Token = Token { getToken :: Text }
    deriving newtype (Eq, Show, ToJSON)

instance Default Token where
    def = Token ""
