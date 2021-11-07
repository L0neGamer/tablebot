{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Tablebot.Plugin.SmartCommand
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Tablebot.Plugin.SmartCommand where

import Data.Proxy
import Data.Text (Text, pack)
import Data.Void (Void)
import Discord.Types (Message)
import GHC.TypeLits
import Tablebot.Plugin.Parser
import Tablebot.Plugin.SmartCommandTH
import Tablebot.Plugin.Types (DatabaseDiscord, Parser)
import Text.Megaparsec

-- | @PComm@ defines function types that we can automatically turn into parsers
-- by composing a parser per input of the function provided.
-- For example, @Int -> Maybe Text -> Message -> DatabaseDiscord ()@ builds a
-- parser that reads in an @Int@, then some optional @Text@, and then uses
-- those to run the provided function with the arguments parsed and the message
-- itself.
class PComm commandty where
  parseComm :: commandty -> Parser (Message -> DatabaseDiscord ())

-- As a base case, remove the spacing and check for eof.
instance {-# OVERLAPPING #-} PComm (Message -> DatabaseDiscord ()) where
  parseComm comm = skipSpace >> eof >> return comm

-- Second base case is the single argument - no trailing space is wanted so we
-- have to specify this case.
instance {-# OVERLAPPING #-} CanParse a => PComm (a -> Message -> DatabaseDiscord ()) where
  parseComm comm = do
    this <- pars @a
    parseComm (comm this)

-- Recursive case is to parse the domain of the function type, then the rest.
instance {-# OVERLAPPABLE #-} (CanParse a, PComm as) => PComm (a -> as) where
  parseComm comm = do
    this <- pars @a
    space
    parseComm (comm this)

-- | @FromString@ defines types that can be retrieved from @String@, which is
-- the usual result of the common parsers.
-- Note that this is a pure function not a parser, so needs to always succeed
-- (hence why we don't have instances like @FromString Int@).
class FromString x where
  fromString :: String -> x

instance FromString String where
  fromString = id

instance FromString Text where
  fromString = pack

-- | @CanParse@ defines types from which we can generate parsers.
class CanParse a where
  pars :: Parser a

-- Note: since FromString and (Read, Integral) can overlap, we cannot specify
-- this instance as FromString a => CanParse a.
instance CanParse Text where
  pars = pack <$> word

-- This overlaps CanParse [a], since String = [Char].
instance {-# OVERLAPPING #-} CanParse String where
  pars = word

-- | @Quoted a@ defines an input of type @a@ that is contained within quotes.
newtype Quoted a = Qu a

instance FromString a => CanParse (Quoted a) where
  pars = Qu . fromString <$> quoted

-- A parser for @Maybe a@ attempts to parse @a@, returning @Just x@ if
-- correctly parsed, else @Nothing@.
instance CanParse a => CanParse (Maybe a) where
  pars = optional $ try (pars @a)

-- A parser for @[a]@ parses any number of @a@s.
instance {-# OVERLAPPABLE #-} CanParse a => CanParse [a] where
  pars = many pars

-- A parser for @Either a b@ attempts to parse @a@, and if that fails then
-- attempts to parse @b@.
instance {-# OVERLAPPABLE #-} (CanParse a, CanParse b) => CanParse (Either a b) where
  pars = (Left <$> pars @a) <|> (Right <$> pars @b)

instance {-# OVERLAPPABLE #-} (CanParse a, CanParse b) => CanParse (EitherS a b) where
  pars = (LeftS <$> pars @a) <|> (RightS <$> pars @b)

instance {-# OVERLAPPING #-} (CanParse a) => CanParse (EitherS a Void) where
  pars = LeftS <$> pars @a

data EitherS a b = LeftS !a | RightS !b

-- AnyOf, which generates a tree of Eithers for commands with many alternatives.
type family AnyOf (xs :: [*]) :: * where
  AnyOf '[] = Void
  AnyOf (x ': xs) = EitherS x (AnyOf xs)

-- Generates patterns for AnyOf accessors.
$(makeChoiceAccessors 10)

-- Various tuple instances.
$(canParseInstances 10)

-- | @Exactly s@ defines an input exactly matching @s@ and nothing else.
data Exactly (s :: Symbol) = Ex

instance KnownSymbol s => CanParse (Exactly s) where
  pars = chunk (pack $ symbolVal (Proxy :: Proxy s)) >> return Ex

-- | @WithError err x@ parses an @x@, reporting @err@ if the parsing of @x@
-- fails.
newtype WithError (err :: Symbol) x = WErr x

instance (KnownSymbol err, CanParse x) => CanParse (WithError err x) where
  pars = (WErr <$> pars @x) <?> symbolVal (Proxy :: Proxy err)

-- | Parsing implementation for all integral types
-- Overlappable due to the really flexible head state
instance {-# OVERLAPPABLE #-} (Integral a, Read a) => CanParse a where
  pars = integer

instance CanParse Double where
  pars = double

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput a = ROI a

instance FromString a => CanParse (RestOfInput a) where
  pars = ROI . fromString <$> untilEnd

-- | @noArguments@ is a type-specific alias for @parseComm@ for commands that
-- have no arguments (thus making it extremely clear).
noArguments :: (Message -> DatabaseDiscord ()) -> Parser (Message -> DatabaseDiscord ())
noArguments = parseComm
