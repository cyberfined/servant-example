module OptionsParser
    ( Options(..)
    , runOptionsParser
    ) where

import Backend (Environment(..))
import System.Environment
import System.Exit

data Options = Options
    { optConfigPath  :: !FilePath
    , optEnvironment :: !Environment
    , optDbMateArgs  :: ![String]
    }

runOptionsParser :: IO Options
runOptionsParser = do
    args <- getArgs
    let (args', envStr) = getEnvStr args
        (args'', configPath) = getConfigPath args'
    env <- parseEnvironment envStr
    pure $ Options { optConfigPath  = configPath
                   , optEnvironment = env
                   , optDbMateArgs  = args''
                   }
  where printHelp :: IO a
        printHelp = do
            progName <- getProgName
            putStrLn $  "Usage: " ++ progName
                     ++ " [-c|--config ARG] [-e|--environment ARG] [dbmate args...]"
            exitFailure

        getEnvStr :: [String] -> ([String], String)
        getEnvStr args
          | Just res <- getArg "-e" args            = res
          | Just res <- getArg "--environment" args = res
          | otherwise                               = (args, "development")

        getConfigPath :: [String] -> ([String], String)
        getConfigPath args
          | Just res <- getArg "-c" args       = res
          | Just res <- getArg "--config" args = res
          | otherwise                          = (args, "config.cfg")

        parseEnvironment :: String -> IO Environment
        parseEnvironment = \case
            "development" -> pure Development
            "test"        -> pure Test
            "production"  -> pure Production
            env           -> do
                putStr $ "Error: wrong environment value " ++ env ++ "\n\n"
                printHelp

        getArg :: String -> [String] -> Maybe ([String], String)
        getArg arg (k:v:as)
          | arg == k  = Just (as, v)
          | otherwise = (\(resAs, res) -> (k:v:resAs, res)) <$> getArg arg as
        getArg _ _ = Nothing
