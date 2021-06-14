{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Backend.Common
    ( Column
    , Create
    , Update
    , Timestamp(..)
    , jsonStrip
    , jsonRoot

    , use
    , statement
    , runSessionIO
    , runStatementIO
    , logStatementIO
    , whenNothing
    , whenLeft

    , module Data.Default.Class
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char                  (toLower)
import Data.Default.Class
import Data.List                  (stripPrefix)
import Data.Time.Clock            (UTCTime(..))
import Database.Hasqul
import Hasql.Pool                 (UsageError, acquire, release, use)
import Hasql.Session              (Session, statement)
import Hasql.Statement            (Statement(..))
import Servant.API                (FromHttpApiData)

import Backend.AppM.LoggerContext

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as Text
import qualified Hasql.Pool            as Pool

deriving newtype instance FromHttpApiData (Key a)

data Create
data Update

type family Column f a where
    Column Create a = a
    Column Update a = Maybe a

newtype Timestamp = Timestamp { getTimestamp :: UTCTime }
    deriving newtype (Eq, Ord, Show, ToJSON, Valuable)
    deriving Codec via ValueCodec Timestamp

instance Default Timestamp where
    def = Timestamp $ UTCTime (toEnum 0) (toEnum 0)

jsonStrip :: String -> Options
jsonStrip prefix =
    defaultOptions { fieldLabelModifier = \s -> maybe s small $ stripPrefix prefix s
                   , omitNothingFields  = True
                   }
  where small (x:xs) = toLower x:xs
        small xs = xs

jsonRoot :: String -> (Value -> Parser a) -> Value -> Parser a
jsonRoot field p = withObject field $ \obj -> obj .: (Text.pack field) >>= p

runSessionIO :: Pool.Settings -> Session a -> IO (Either UsageError a)
runSessionIO poolSettings session = do
    pool <- acquire poolSettings
    res <- use pool session
    release pool
    pure res

runStatementIO :: Show a
               => Pool.Settings
               -> Maybe LoggerContext
               -> a
               -> Statement a b
               -> IO (Either UsageError b)
runStatementIO poolSettings loggerCtx params stat = do
    logStatementIO loggerCtx params stat
    runSessionIO poolSettings (statement params stat)

logStatementIO :: Show a => Maybe LoggerContext -> a -> Statement a b -> IO ()
logStatementIO loggerCtx a (Statement sql _ _ _) = writeLogIO loggerCtx msg
  where msg = sql <> " (" <> BS.pack (show a) <> ")"

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing m f = maybe f pure m

whenLeft :: Applicative f => Either a b -> (a -> f b) -> f b
whenLeft e f = either f pure e
