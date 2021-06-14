module Backend.User.Validation
    ( UserValidation
    , UserValidationError(..)
    , validateLogin
    , validateEmail
    , validatePassword
    , module Validation
    ) where

import Control.Applicative (liftA2)
import Data.Aeson
import Data.Maybe          (isJust)
import Data.Text           (Text)
import Data.Text.Encoding  (encodeUtf8)
import Validation          (Validation)

import Backend.AppM
import Backend.KeyError
import Backend.User.Model
import Backend.User.Statements

import qualified Data.Text           as Text
import qualified Text.Email.Validate as Email

type UserValidation = Validation (KeyErrors UserValidationError)

data UserValidationError = LoginIsShort
                         | LoginIsUsed
                         | PasswordIsShort
                         | EmailIsInvalid
                         | EmailIsUsed

instance ToJSON UserValidationError where
    toJSON = String . \case
        LoginIsShort    -> "is too short"
        LoginIsUsed     -> "is already used"
        PasswordIsShort -> "is too short"
        EmailIsInvalid  -> "is invalid"
        EmailIsUsed     -> "is already used"

validateLogin :: AppM m => Text -> m (UserValidation Login)
validateLogin login = liftA2 (*>) (pure validateLoginIsShort) validateLoginIsUsed
  where validateLoginIsShort =  Login login
                             <$ failureIf (Text.length login < 3) loginIsShort
        validateLoginIsUsed = do
            mUser <- runSession (session (Login login) userByLogin)
            pure $ Login login <$ failureIf (isJust mUser) loginIsUsed
        loginIsShort = KeyError "username" LoginIsShort
        loginIsUsed = KeyError "username" LoginIsUsed

validateEmail :: AppM m => Text -> m (UserValidation Email)
validateEmail email = liftA2 (*>) validateEmailIsUsed (pure validateEmailIsInvalid)
  where bsEmail = encodeUtf8 email
        validateEmailIsUsed = do
            mUser <- runSession (session (Email email) userByEmail)
            pure $ Email email <$ failureIf (isJust mUser) emailIsUsed
        validateEmailIsInvalid =  Email email
                               <$ failureUnless (Email.isValid bsEmail) emailIsInvalid
        emailIsUsed = KeyError "email" EmailIsUsed
        emailIsInvalid = KeyError "email" EmailIsInvalid

validatePassword :: Text -> UserValidation Password
validatePassword pass = Password pass <$ failureIf (Text.length pass < 8) passwordIsShort
  where passwordIsShort = KeyError "password" PasswordIsShort
