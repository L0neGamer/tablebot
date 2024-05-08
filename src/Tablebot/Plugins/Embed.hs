-- |
-- Module      : Tablebot.Plugins.Embed
-- Description : A command that embeds a message
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Tablebot.Plugins.Embed (embeds) where

import qualified Data.Text as T
import Tablebot.Utility
import Tablebot.Utility.Discord (Message, sendEmbedMessage)
import Tablebot.Utility.Parser
import Tablebot.Utility.Permission (requirePermission)
import Text.RawString.QQ
import Prelude hiding (flip)

-- | Sends an embedded message
embed :: Command
embed = Command "embed" embedcomm []
  where
    embedcomm :: Parser (Message -> DatabaseDiscord ())
    embedcomm = do
      args <- quoteWith "```" "```"
      return $ \m ->
        requirePermission Superuser m $ sendEmbedMessage m (T.pack "") (T.pack args)

embedHelp :: HelpPage
embedHelp =
  HelpPage
    "embed"
    []
    "embed a message"
    [r|**Embed**
Create an embed with the given text

*Usage:*
`embed ```Some text``` `
|]
    []
    Superuser

embedPlugin :: Plugin
embedPlugin = (plug "embed") {commands = [embed], helpPages = [embedHelp]}

embeds :: CompiledPlugin
embeds = compilePlugin embedPlugin
