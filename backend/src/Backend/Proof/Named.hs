{-# LANGUAGE RankNTypes #-}

module Backend.Proof.Named
    ( Named
    , name
    , unName
    ) where

newtype Named name a = Named a

unName :: Named name a -> a
unName (Named a) = a

name :: a -> (forall name. Named name a -> t) -> t
name a f = f (Named a)
