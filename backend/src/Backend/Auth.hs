{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Auth
    ( AuthApi
    , AuthResult
    , JwtProtect

    , getAuthResult
    , genAuthServerContext
    , authHandlers
    , passwordHash
    , checkPasswordHash
    , module Backend.Auth.AuthContext
    ) where

import Control.Monad.Reader
import Control.Monad.Except
import Crypto.Auth.JWT
import Crypto.Hash                      (hashWith)
import Crypto.Hash.Algorithms
import Data.Aeson
import Data.ByteString                  (ByteString, stripPrefix)
import Data.ByteArray                   (convert)
import Data.Char                        (isSpace)
import Data.Proxy
import Data.Text                        (Text)
import Data.Text.Encoding               (encodeUtf8, decodeUtf8)
import Database.Hasqul                  (Key(..))
import GHC.Generics
import Network.Wai                      (Request, requestHeaders)
import Servant.API
import Servant.Server
import Servant.Server.Experimental.Auth

import Backend.AppM
import Backend.Auth.AuthContext
import Backend.Common
import Backend.Response
import Backend.User.Model hiding (Token(..))
import Backend.User.Statements
import Backend.User.Validation

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as Text

import qualified Backend.User.Model    as User

type AuthApi =   "users" :> ReqBody '[JSON] SignUp :> Post '[JSON] User
            :<|> "users" :> "login" :> ReqBody '[JSON] SignIn :> Post '[JSON] User

authHandlers :: AppM m => ServerT AuthApi m
authHandlers = signUpUser :<|> signInUser

newtype AuthPayload = AuthPayload { authUserId :: Key User }
    deriving newtype (ToJSON, FromJSON)

type JwtProtect = AuthProtect "jwt-auth"

newtype AuthResult a = AuthResult a

getAuthResult :: AuthResult a -> a
getAuthResult (AuthResult a) = a

type instance AuthServerData JwtProtect = AuthResult User

genAuthServerContext :: AppM m
                     => Proxy m
                     -> AppContext
                     -> Context '[AuthHandler Request (AuthResult User)]
genAuthServerContext monadPrx appCtx = authHandler monadPrx appCtx :. EmptyContext

authHandler :: forall m. AppM m
            => Proxy m
            -> AppContext
            -> AuthHandler Request (AuthResult User)
authHandler _ appCtx = mkAuthHandler handler
  where handler :: Request -> Handler (AuthResult User)
        handler req = runAppM @m appCtx $ do
            AuthContext{..} <- asks appCtxAuthCtx
            writeLog "Authorization try"
            tok <- whenNothing (extractToken req) (throwError tokenNotFound)
            eitherTok <- payload authJWT tok
            userId <- authUserId <$> whenLeft eitherTok (throwError . jwtToServerError)
            mUser <- runSession (session userId userById)
            user <- setUserToken tok <$> whenNothing mUser (throwError userNotFound)
            writeLog $ "User " <> BS.pack (show userId) <> " was authorized"
            pure $ AuthResult user

extractToken :: Request -> Maybe Token
extractToken req = do
    authContent <- lookup "Authorization" (requestHeaders req)
    Token . strip <$> stripPrefix "Token" authContent

data SignUp = SignUp
    { signUpEmail    :: !Text
    , signUpUsername :: !Text
    , signUpPassword :: !Text
    } deriving Generic

instance FromJSON SignUp where
    parseJSON = jsonRoot "user" $ genericParseJSON (jsonStrip "signUp")

data SignIn = SignIn
    { signInEmail    :: !Text
    , signInPassword :: !Text
    } deriving Generic

instance FromJSON SignIn where
    parseJSON = jsonRoot "user" $ genericParseJSON (jsonStrip "signIn")

signUpUser :: AppM m => SignUp -> m User
signUpUser SignUp{..} = do
    AuthContext { authJWT = jwt } <- asks appCtxAuthCtx
    vUser <- mUser
    user <- validateM vUser err422
    uid <- runSession (session user insertUser)
    tok <- token jwt (AuthPayload uid)
    pure $ setUserToken tok user
  where mUser = do
            AuthContext { authHash = hash } <- asks appCtxAuthCtx
            vLogin <- validateLogin signUpUsername
            vEmail <- validateEmail signUpEmail
            let vPassword = passwordHash hash <$> validatePassword signUpPassword
            pure $  User <$> pure def <*> vEmail <*> vLogin <*> vPassword <*> pure def
                <*> pure def <*> pure def <*> pure def

signInUser :: AppM m => SignIn -> m User
signInUser SignIn{..} = do
    AuthContext { authJWT = jwt, authHash = hash } <- asks appCtxAuthCtx
    let pHash = passwordHash hash $ Password signInPassword
        email = Email signInEmail
    mUser <- runSession (session (email, pHash) userByEmailPass)
    user <- whenNothing mUser (throwError wrongEmailOrPasswordErr)
    tok <- token jwt (AuthPayload $ userId user)
    pure $ setUserToken tok user
  where wrongEmailOrPasswordErr = jsonError err401 "Wrong email or password"

checkPasswordHash :: AppM m => User -> Password -> m Bool
checkPasswordHash User{..} pass = do
    AuthContext { authHash = hash } <- asks appCtxAuthCtx
    let pHash = passwordHash hash pass
    pure $ pHash == userPasswordHash

passwordHash :: HashAlgorithm a => a -> Password -> PasswordHash
passwordHash alg = PasswordHash . convert . hashWith alg . encodeUtf8 . getPassword

strip :: ByteString -> ByteString
strip = BS.takeWhile (not . isSpace) . BS.dropWhile isSpace

jwtToServerError :: JWTError -> ServerError
jwtToServerError err = jsonError err401 (Text.pack $ show err)

tokenNotFound :: ServerError
tokenNotFound = jsonError err401 "Can't find an authorization token"

userNotFound :: ServerError
userNotFound = jsonError err401 "User was deleted"

setUserToken :: Token -> User -> User
setUserToken (Token tok) user = user { userToken = User.Token $ decodeUtf8 tok }
