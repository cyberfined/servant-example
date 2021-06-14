module Main (main) where

import Backend
import Data.Proxy
import Options.Applicative

import OptionsParser

main :: IO ()
main = do
    Options{..} <- execParser optionsParser
    runBackend optConfigPath optEnvironment (Proxy :: Proxy AppMonad)
