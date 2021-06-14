{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.AppM.Session
    ( Transaction
    , Session
    , runSession
    , session
    , runTransaction
    , transact
    , logStatement
    , logStatementIO
    ) where

import Database.Hasqul.Updater
import Hasql.Statement
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)

import Backend.AppM.Monad
import Backend.AppM.Logger
import Backend.Common
import Backend.Response

import qualified Data.ByteString.Char8   as BS
import qualified Hasql.Session           as Session
import qualified Hasql.Transaction       as Transaction

newtype Session m a = Session
    { getSession :: Session.Session (m (), a)
    } deriving Functor

instance AppM m => Applicative (Session m) where
    pure x = Session (pure (pure (), x))
    (Session fs) <*> (Session x) = Session $
        fmap (\(l1, f) (l2, a) -> (l1 >> l2, f a)) fs <*> x

instance AppM m => Monad (Session m) where
    (Session ma) >>= fm = Session $ do
        (l1, a) <- ma
        (l2, b) <- getSession $ fm a
        pure (l1 >> l2, b)

instance (Show a, Updatable a, AppM m) => FromStatement a (Session m) where
    update kx x = case updateStatement x of
        Nothing -> pure ()
        Just st -> session (kx, x) st

runSession :: AppM m => Session m a -> m a
runSession (Session s) = do
    (logger, res) <- withSession s
    logger
    pure res

withSession :: AppM m => Session.Session a -> m a
withSession s = do
    poolSettings <- asks appCtxPoolSetings
    eRes <- liftIO $ runSessionIO poolSettings s
    whenLeft eRes $ \err -> do
        writeLog (BS.pack $ show err)
        throwError internalError

session :: (Show a, AppM m) => a -> Statement a b -> Session m b
session params stat = Session $ do
    res <- Session.statement params stat
    pure (logStatement params stat, res)

newtype Transaction m a = Transaction
    { getTransaction :: Transaction.Transaction (m (), a)
    } deriving Functor

instance AppM m => Applicative (Transaction m) where
    pure x = Transaction (pure (pure (), x))
    (Transaction ft) <*> (Transaction x) = Transaction $
        fmap (\(l1, f) (l2, a) -> (l1 >> l2, f a)) ft <*> x

instance AppM m => Monad (Transaction m) where
    (Transaction ma) >>= fm = Transaction $ do
        (l1, a) <- ma
        (l2, b) <- getTransaction $ fm a
        pure (l1 >> l2, b)

instance (Show a, Updatable a, AppM m) => FromStatement a (Transaction m) where
    update kx x = case updateStatement x of
        Nothing -> pure ()
        Just st -> transact (kx, x) st

runTransaction :: AppM m => Transaction m a -> m a
runTransaction (Transaction t) = do
    (logger, res) <- withSession (transaction Serializable Write t)
    writeLog "BEGIN TRANSACTION"
    logger
    writeLog "END TRANSACTION"
    pure res

transact :: (Show a, AppM m) => a -> Statement a b -> Transaction m b
transact params stat = Transaction $ do
    res <- Transaction.statement params stat
    pure (logStatement params stat, res)

logStatement :: (Show a, AppM m) => a -> Statement a b -> m ()
logStatement a stat = do
    loggerCtx <- asks appCtxLoggerCtx
    liftIO $ logStatementIO loggerCtx a stat
