module Backend.User.Statements
    ( userById
    , userByEmailPass
    , userByLogin
    , userByEmail
    , insertUser
    ) where

import Database.Hasqul
import Hasql.Statement

import Backend.User.Model

userById :: Statement (Key User) (Maybe User)
userById = Statement sql (encode @(Key User)) (decode @(Maybe User)) False
  where sql = "SELECT * FROM users WHERE id=$1"

userByEmailPass :: Statement (Email, PasswordHash) (Maybe User)
userByEmailPass = Statement sql enc dec False
  where sql = "SELECT * FROM users WHERE email = $1 AND password_hash = $2"
        enc = encode @(Email, PasswordHash)
        dec = decode @(Maybe User)

userByLogin :: Statement Login (Maybe User)
userByLogin = Statement sql (encode @Login) (decode @(Maybe User)) False
  where sql = "SELECT * FROM users WHERE login = $1"

userByEmail :: Statement Email (Maybe User)
userByEmail = Statement sql (encode @Email) (decode @(Maybe User)) False
  where sql = "SELECT * FROM users WHERE email = $1"

insertUser :: Statement User (Key User)
insertUser = Statement sql (encode @User) (decode @(Key User)) False
  where sql = "INSERT INTO users (email, login, password_hash, image, bio) \
              \VALUES ($1, $2, $3, $4, $5) RETURNING id"
