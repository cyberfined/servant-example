module Backend.KeyError
    ( KeyErrors
    , KeyError(..)
    , failureIf
    , failureUnless
    ) where

import Data.Aeson          (ToJSON(..))
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty  (NonEmpty(..))
import Data.Text           (Text)
import Validation          (Validation(..))

import qualified Data.HashMap.Strict as HashMap

newtype KeyErrors a = KeyErrors (HashMap Text (NonEmpty a))
    deriving newtype ToJSON

instance Semigroup (KeyErrors a) where
    (KeyErrors a) <> (KeyErrors b) = KeyErrors (HashMap.unionWith (<>) a b)

data KeyError a = KeyError !Text !a

failureIf :: Bool -> KeyError a -> Validation (KeyErrors a) ()
failureIf cond (KeyError key err)
  | cond      = Failure (KeyErrors $ HashMap.singleton key (err :| []))
  | otherwise = Success ()

failureUnless :: Bool -> KeyError a -> Validation (KeyErrors a) ()
failureUnless cond = failureIf (not cond)
