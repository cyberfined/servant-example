module Backend.User.Handler
    ( UserApi
    , userHandlers
    ) where

import Servant.API
import Servant.Server

import Backend.Auth
import Backend.AppM
import Backend.User.Model

type UserApi = JwtProtect :>
    ( "user" :> Get '[JSON] User
    )

userHandlers :: AppM m => ServerT UserApi m
userHandlers auth = userHandler auth

userHandler :: AppM m => AuthResult User -> m User
userHandler = pure . getAuthResult
