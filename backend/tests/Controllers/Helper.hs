module Controllers.Helper
    ( withApp
    , runSession
    , runStatement
    , getAuth
    , post
    , clearDb
    , jsonStrip
    , jsonRoot
    , SResponse(..)
    , TestContext(..)

    , module Control.Monad
    , module Data.Default.Class
    , module Network.HTTP.Types.Status
    , module Test.Hspec
    , module Test.Hspec.Wai
    , module Test.Hspec.Wai.JSON
    , module TimeFreezeM
    ) where

import Backend                   (backendApp)
import Backend.Config
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString           (ByteString)
import Data.Char                 (toLower)
import Data.Default.Class
import Data.List                 (stripPrefix)
import Data.Proxy
import Data.Text                 (Text)
import Hasql.Pool
import Hasql.Session      hiding (sql)
import Hasql.Statement
import Hasql.Transaction         (Transaction)
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai               (Application)
import Network.Wai.Test          (SResponse(..))
import Prelude            hiding (truncate)
import Test.Hspec
import Test.Hspec.Wai     hiding (post, pending, pendingWith)
import Test.Hspec.Wai.JSON

import TimeFreezeM

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Hasql.Decoders             as Dec
import qualified Hasql.Encoders             as Enc
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction

newtype TestContext = TestContext
    { testCtxPoolSettings :: Settings
    }

newtype ConnectionException = ConnectionException
    { getConnectionException :: Maybe ByteString
    }

instance Show ConnectionException where
    show = maybe "no details" (BS.unpack) . getConnectionException

instance Exception ConnectionException

withApp :: SpecWith (TestContext, Application) -> Spec
withApp = withState state . after clearDb

state :: IO (TestContext, Application)
state = do
    poolSettings <- cfgDbPoolSettings <$> loadAppConfig cfgPath env
    app <- fst <$> backendApp cfgPath env (Proxy @TimeFreezeM)
    pure (TestContext poolSettings, app)
  where cfgPath = "config.cfg"
        env = Test

runSession :: Session a -> WaiSession TestContext a
runSession session = do
    testCtx <- getState
    liftIO $ runSessionCtx testCtx session

runStatement :: Statement a b -> a -> WaiSession TestContext b
runStatement stmt a = runSession (statement a stmt)

runSessionCtx :: TestContext -> Session a -> IO a
runSessionCtx TestContext{..} session =
    bracket (acquire testCtxPoolSettings) release $ \pool -> do
        use pool session >>= \case
            Left err  -> case err of
                ConnectionError conErr -> throwIO (ConnectionException conErr)
                SessionError queryErr  -> throwIO queryErr
            Right res -> pure res

authQuery :: Method
          -> BS.ByteString
          -> Text
          -> BL.ByteString
          -> WaiSession TestContext SResponse
authQuery method path token = request method path headers
    where headers =  [ ("Content-Type", "application/json")
                     , ("Authorization", "Token " <> (Text.encodeUtf8 token))
                     ]

getAuth :: BS.ByteString
        -> Text
        -> BL.ByteString
        -> WaiSession TestContext SResponse
getAuth = authQuery methodGet

post :: BS.ByteString -> BL.ByteString -> WaiSession TestContext SResponse
post path = request methodPost path headers
    where headers =  [("Content-Type", "application/json")]

clearDb :: (TestContext, Application) -> IO ()
clearDb (testCtx, _) = runSessionCtx testCtx session
  where session = Transaction.transaction isoLevel mode $ mapM_ truncate tables
        tables :: [ByteString]
        tables = [ "users" ]

        truncate :: ByteString -> Transaction ()
        truncate table = Transaction.statement () stmt
          where sql = "TRUNCATE " <> table <> " CASCADE"
                stmt = Statement sql Enc.noParams Dec.noResult False

        isoLevel :: Transaction.IsolationLevel
        isoLevel = Transaction.Serializable

        mode :: Transaction.Mode
        mode = Transaction.Write

jsonStrip :: String -> Options
jsonStrip prefix =
    defaultOptions { fieldLabelModifier = \s -> maybe s small $ stripPrefix prefix s
                   , omitNothingFields  = True
                   }
  where small (x:xs) = toLower x:xs
        small xs = xs

jsonRoot :: String -> (Value -> Parser a) -> Value -> Parser a
jsonRoot field p = withObject field $ \obj -> obj .: (Text.pack field) >>= p
