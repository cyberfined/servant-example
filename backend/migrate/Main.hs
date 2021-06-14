{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Backend            (envSubConfig)
import Control.Monad      (void)
import Data.Configurator
import Data.Word          (Word16)
import System.Process

import OptionsParser

data DbConfig = DbConfig
    { dbHost          :: !String
    , dbPort          :: !Word16
    , dbUser          :: !String
    , dbPassword      :: !String
    , dbDatabase      :: !String
    , dbMatePath      :: !FilePath
    , dbMigrationsDir :: !FilePath
    , dbMateArgs      :: ![String]
    }

loadDbConfig :: Options -> IO DbConfig
loadDbConfig Options{..} = do
    mainCfg <- envSubConfig optEnvironment <$> load [Required optConfigPath]
    let dbCfg = subconfig "db" mainCfg
        dbMateCfg = subconfig "dbMate" mainCfg
    DbConfig <$> require dbCfg "host"
             <*> require dbCfg "port"
             <*> require dbCfg "user"
             <*> require dbCfg "password"
             <*> require dbCfg "database"
             <*> require dbMateCfg "path"
             <*> require dbMateCfg "migrationsDir"
             <*> pure optDbMateArgs

runDbMate :: DbConfig -> IO ()
runDbMate DbConfig{..} = do
    handle <- runProcess dbMatePath args Nothing Nothing Nothing Nothing Nothing
    void $ waitForProcess handle
  where dbUrl =  "postgresql://" ++ dbUser ++ ":" ++ dbPassword
              ++ "@" ++ dbHost ++ ":" ++ show dbPort ++ "/" ++ dbDatabase
              ++ "?sslmode=disable"
        args = ["--url", dbUrl, "--migrations-dir", dbMigrationsDir] ++ dbMateArgs

main :: IO ()
main = runOptionsParser >>= loadDbConfig >>= runDbMate
