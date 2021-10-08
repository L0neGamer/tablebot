module Main where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import Tablebot (runTablebot)
import Tablebot.Plugins (plugins)

main :: IO ()
main = do
  loadEnv
  dToken <- pack <$> getEnv "DISCORD_TOKEN"
  prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
  dbpath <- getEnv "SQLITE_FILENAME"
  runTablebot dToken prefix dbpath plugins
