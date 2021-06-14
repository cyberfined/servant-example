{-# LANGUAGE ExistentialQuantification #-}

module Backend.Auth.AuthContext (AuthContext(..)) where

import Crypto.Auth.JWT
import Crypto.Hash.Algorithms

data AuthContext = forall a. (HashAlgorithm a, Show a) =>
    AuthContext { authJWT  :: !(JWT a)
                , authHash :: !a
                }
