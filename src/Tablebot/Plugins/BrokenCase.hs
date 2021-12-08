-- |
-- Module      : Tablebot.Plugins.BrokenCase
-- Description : na
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- na
module Tablebot.Plugins.BrokenCase (brokenPlugin) where

import Data.Text (Text, pack)
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.SmartCommand (parseComm)
import Tablebot.Plugin.Types
  ( Command,
    DatabaseDiscord,
    EnvCommand (Command),
    EnvPlugin (..),
    Plugin,
    plug,
  )

cat :: Command
cat =
  Command
    "example"
    (parseComm echo)
    []
  where
    echo :: Maybe Integer -> Maybe Text -> Message -> DatabaseDiscord ()
    echo i t m = do
      let r = pack $ "i is: " <> show i <>"\nt is: " <> show t
      sendMessage m r

brokenPlugin :: Plugin
brokenPlugin = (plug "broken") {commands = [cat], helpPages = []}
