{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.AppM.Monad
    ( AppM(..)
    , AppMonad(..)
    , AppContext(..)

    , asks
    , liftIO
    , throwError
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Time
import Hasql.Pool                 (Settings)
import Servant.Server             (Handler, ServerError)

import Backend.Auth.AuthContext
import Backend.AppM.LoggerContext

class (MonadIO m, MonadReader AppContext m, MonadError ServerError m, MonadTime m)
  => AppM m where
    runAppM :: AppContext -> m a -> Handler a

newtype AppMonad a = AppMonad (ReaderT AppContext Handler a)
    deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadReader AppContext, MonadError ServerError
                     )

instance MonadTime AppMonad where
    currentTime = liftIO currentTime

instance AppM AppMonad where
    runAppM ctx (AppMonad ma) = runReaderT ma ctx

data AppContext = AppContext
    { appCtxPoolSetings :: !Settings
    , appCtxAuthCtx     :: !AuthContext
    , appCtxLoggerCtx   :: !(Maybe LoggerContext)
    }
