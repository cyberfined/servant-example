module Backend.Config
    ( AppConfig(..)
    , LogConfig(..)
    , LogFile(..)
    , Environment(..)
    , loadAppConfig
    , envSubConfig
    ) where

import Crypto.Auth.JWT
import Crypto.Hash.Algorithms
import Data.Configurator
import Data.Configurator.Types
import Data.Text               (Text)
import Data.Time.Clock         (NominalDiffTime)
import Hasql.Pool              (Settings)
import Hasql.Connection        (settings)
import System.Log.FastLogger   (BufSize)

import Backend.Auth.AuthContext

import qualified Data.Text      as Text
import qualified Data.Text.Read as Text

data AppConfig = AppConfig
    { cfgDbPoolSettings :: !Settings
    , cfgAuthCtx        :: !AuthContext
    , cfgLog            :: !(Maybe LogConfig)
    , cfgPort           :: !Int
    , cfgEnvironment    :: !Environment
    }

data Environment
    = Development
    | Production
    | Test

instance Show Environment where
    show = \case
        Development -> "development"
        Production  -> "production"
        Test        -> "test"

loadAppConfig :: FilePath -> Environment -> IO AppConfig
loadAppConfig configPath env = do
    mainCfg <- envSubConfig env <$> load [Required configPath]
    let appCfg = subconfig "app" mainCfg
        dbCfg = subconfig "db" mainCfg
        jwtCfg = subconfig "jwt" mainCfg
        logCfg = subconfig "log" mainCfg
    AppConfig <$> requireDbPoolSettings dbCfg
              <*> requireAuthCtx jwtCfg
              <*> requireLogConfig logCfg
              <*> require appCfg "port"
              <*> pure env

requireDbPoolSettings :: Config -> IO Settings
requireDbPoolSettings dbCfg = do
    poolSize <- require dbCfg "poolSize"
    dbSettings <- requireDbSettings
    pure (poolSize, durationToDiffTime timeout, dbSettings)
  where timeout = Duration { days    = 0
                           , hours   = 0
                           , minutes = 0
                           , seconds = 30
                           }
        requireDbSettings =  settings
                         <$> require dbCfg "host"
                         <*> require dbCfg "port"
                         <*> require dbCfg "user"
                         <*> require dbCfg "password"
                         <*> require dbCfg "database"

requireAuthCtx :: Config -> IO AuthContext
requireAuthCtx authCfg =  AuthContext
                      <$> requireJwt
                      <*> pure SHA256
  where requireJwt =  initJwt SHA256
                  <$> (Secret <$> require authCfg "secret")
                  <*> (durationToDiffTime <$> require authCfg "duration")

data LogConfig = LogConfig
    { logCfgFile    :: !LogFile
    , logCfgBufSize :: !BufSize
    }

data LogFile
    = LogFile !FilePath
    | LogStdout
    | LogStderr

requireLogConfig :: Config -> IO (Maybe LogConfig)
requireLogConfig logCfg = do
    isDisabled <- lookupDefault False logCfg "disabled"
    if isDisabled
       then pure Nothing
       else Just <$> (LogConfig <$> requireFile <*> require logCfg "bufSize")
  where requireFile = require logCfg "file" >>= \case
            "stdout" -> pure LogStdout
            "stderr" -> pure LogStderr
            filePath -> pure $ LogFile filePath

envSubConfig :: Environment -> Config -> Config
envSubConfig = subconfig . Text.pack . show

data Duration = Duration
    { days    :: !Int
    , hours   :: !Int
    , minutes :: !Int
    , seconds :: !Int
    }

instance Configured Duration where
    convert = \case
        String str -> parseDuration str
        _          -> Nothing
      where parseDuration :: Text -> Maybe Duration
            parseDuration str
              | [d, h, m] <- Text.strip <$> Text.splitOn ":" str
              =   Duration
              <$> toMaybe (fst <$> Text.decimal d)
              <*> toMaybe (fst <$> Text.decimal h)
              <*> toMaybe (fst <$> Text.decimal m)
              <*> pure 0
              | otherwise
              = Nothing
            toMaybe :: Either a b -> Maybe b
            toMaybe = either (const Nothing) Just

durationToDiffTime :: Duration -> NominalDiffTime
durationToDiffTime Duration{..} = fromIntegral days * dSecs
                                + fromIntegral hours * hSecs
                                + fromIntegral minutes * mSecs
                                + fromIntegral seconds
  where dSecs = 86400
        hSecs = 3600
        mSecs = 60
