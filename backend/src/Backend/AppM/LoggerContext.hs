module Backend.AppM.LoggerContext
    ( LoggerContext(..)
    , initLoggerContext
    , writeLogIO
    ) where

import Data.ByteString              (ByteString)
import System.Directory
import System.FilePath
import System.Log.FastLogger hiding (LogType'(..))

import Backend.Config               (LogConfig(..), LogFile(..))

data LoggerContext = LoggerContext
    { loggerCtxLoggerSet  :: !LoggerSet
    , loggerCtxGetFmtTime :: !(IO FormattedTime)
    }

initLoggerContext :: Maybe LogConfig -> IO (Maybe LoggerContext)
initLoggerContext (Just LogConfig{..}) = 
    Just <$> (LoggerContext <$> loggerSet <*> newTimeCache simpleTimeFormat)
  where loggerSet = case logCfgFile of
            LogStdout    -> newStdoutLoggerSet logCfgBufSize
            LogStderr    -> newStderrLoggerSet logCfgBufSize
            LogFile file -> do
                createDirectoryIfMissing True (dropFileName file)
                newFileLoggerSet logCfgBufSize file
initLoggerContext _ = pure Nothing

writeLogIO :: Maybe LoggerContext -> ByteString -> IO ()
writeLogIO (Just LoggerContext{..}) msg = do
    time <- loggerCtxGetFmtTime
    pushLogStrLn loggerCtxLoggerSet ("[" <> toLogStr time <> "] " <> toLogStr msg)
writeLogIO _ _ = pure ()
