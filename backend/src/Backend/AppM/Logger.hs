module Backend.AppM.Logger (writeLog) where

import Data.ByteString (ByteString)

import Backend.AppM.Monad
import Backend.AppM.LoggerContext

writeLog :: AppM m => ByteString -> m ()
writeLog str = do
    loggerCtx <- asks appCtxLoggerCtx
    liftIO (writeLogIO loggerCtx str)
