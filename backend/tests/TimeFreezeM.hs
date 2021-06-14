{-# LANGUAGE MultiParamTypeClasses #-}

module TimeFreezeM
    ( TimeFreezeM(..)
    , setAppTime
    , modifyAppContext
    , getJwtToken
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Time
import Crypto.Auth.JWT
import Crypto.Hash.Algorithms
import Data.IORef
import Data.Text              (Text)
import Data.Time
import Database.Hasqul        (Key(..))
import Servant.Server
import System.IO.Unsafe

import Backend
import Backend.Auth

import qualified Data.Text.Encoding as Text

newtype TimeFreezeM a = TimeFreezeM
    { runTimeFreezeM :: ReaderT (IORef (UTCTime, AppContext)) Handler a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError ServerError)

instance MonadReader AppContext TimeFreezeM where
    ask = TimeFreezeM (ask >>= liftIO . fmap snd . readIORef)
    local f (TimeFreezeM ma) = TimeFreezeM $ do
        ctxRef <- ask
        (tm, oldCtx) <- liftIO (readIORef ctxRef)
        liftIO $ modifyIORef' ctxRef (const $ (tm, f oldCtx))
        res <- ma
        liftIO $ modifyIORef' ctxRef (const $ (tm, oldCtx))
        pure res

instance MonadTime TimeFreezeM where
    currentTime = TimeFreezeM (ask >>= liftIO . fmap fst . readIORef)

authCtx :: AuthContext
authCtx = AuthContext jwt SHA256

jwt :: JWT SHA256
jwt = initJwt SHA256 (Secret "secret") (secondsToNominalDiffTime 600)

timeVar :: IORef (UTCTime, AppContext)
timeVar = unsafePerformIO $ newIORef (tm, undefined)
  where tm = UTCTime (toEnum 0) (toEnum 0)

setAppTime :: MonadIO m => String -> m ()
setAppTime timeStr = liftIO $ modifyIORef' timeVar (\(_, ctx) -> (tm, ctx))
 where tm = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeStr

modifyAppContext :: MonadIO m => (AppContext -> AppContext) -> m ()
modifyAppContext f = liftIO $ modifyIORef' timeVar (\(tm, ctx) -> (tm, f ctx))

getJwtToken :: MonadIO m => Key a -> m Text
getJwtToken key = do
    time <- liftIO $ fst <$> readIORef timeVar
    pure (Text.decodeUtf8 $ unToken $ tokenPure jwt key time)

instance AppM TimeFreezeM where
    runAppM ctx (TimeFreezeM ma) = do
        liftIO $ modifyIORef' timeVar (\(tm, _) -> (tm, ctxWithJwt))
        runReaderT ma timeVar
      where ctxWithJwt = ctx { appCtxAuthCtx = authCtx }
