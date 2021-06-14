module OptionsParser
    ( Options(..)
    , optionsParser
    ) where

import Options.Applicative

import Backend             (Environment(..))

data Options = Options
    { optConfigPath  :: !FilePath
    , optEnvironment :: !Environment
    }

optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper) fullDesc

options :: Parser Options
options = Options
       <$> strOption (  long "config"
                     <> short 'c'
                     <> value "config.cfg"
                     <> showDefault
                     <> help "path to config file"
                     )
       <*> option parseEnv (  long "environment"
                           <> short 'e'
                           <> value Development
                           <> showDefault
                           <> help "application environment"
                           )

parseEnv :: ReadM Environment
parseEnv = eitherReader $ \case
    "development" -> Right Development
    "test"        -> Right Test
    "production"  -> Right Production
    env           -> Left $ "wrong env value " ++ env
