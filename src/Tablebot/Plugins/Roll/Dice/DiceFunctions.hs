-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceFunctions
-- Description : Functions, data, and type classes to deal with functions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Type classes, data, and functions that deal with functions when evaluating
-- dice.
module Tablebot.Plugins.Roll.Dice.DiceFunctions
  ( integerFunctionsList,
    integerFunctions,
    listFunctionsList,
    listFunctions,
    FuncInfoBase (..),
    FuncInfo,
    ListInteger (..),
    ArgType (..),
  )
where

import Control.Monad.Exception (MonadException)
import Data.List (genericDrop, genericLength, genericTake, sort)
import Data.Map as M (Map, fromList, keys)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Tablebot.Utility.Exception (BotException (EvaluationException), throwBot)

-- | The limit to how big a factorial value is permitted. Notably, the factorial
-- function doesn't operate above this limit.
factorialLimit :: Integer
factorialLimit = 50

-- Mappings for what functions are supported

-- | Mapping from function names to the functions themselves for integer
-- functions.
integerFunctions :: MonadException m => Map Text (FuncInfo m)
integerFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) integerFunctions'

-- | The names of the integer functions currently supported.
integerFunctionsList :: [Text]
integerFunctionsList = M.keys (integerFunctions @IO)

-- | The base details of the integer functions, containing all the information
-- for each function that returns an integer.
integerFunctions' :: MonadException m => [FuncInfo m]
integerFunctions' =
  funcInfoIndex :
  constructFuncInfo "length" (genericLength @Integer @Integer) :
  constructFuncInfo "sum" (sum @[] @Integer) :
  constructFuncInfo "maximum" (maximum @[] @Integer) :
  constructFuncInfo "minimum" (minimum @[] @Integer) :
  constructFuncInfo' "mod" (mod @Integer) (Nothing, Nothing, (== 0)) :
  constructFuncInfo' "fact" fact (Nothing, Just factorialLimit, const False) :
  (uncurry constructFuncInfo <$> [("abs", abs @Integer), ("id", id), ("neg", negate)])
  where
    fact n
      | n < 0 = 0
      | n == 0 = 1
      | n > factorialLimit = fact factorialLimit
      | otherwise = n * fact (n - 1)

-- | Mapping from function names to the functions themselves for list functions.
listFunctions :: MonadException m => Map Text (FuncInfoBase m [Integer])
listFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) listFunctions'

-- | The names of the list functions currently supported.
listFunctionsList :: [Text]
listFunctionsList = M.keys (listFunctions @IO)

-- | The base details of the list functions, containing all the information for
-- each function that returns an integer.
listFunctions' :: MonadException m => [FuncInfoBase m [Integer]]
listFunctions' =
  constructFuncInfo @[Integer] "drop" (genericDrop @Integer) :
  constructFuncInfo "take" (genericTake @Integer) :
  (uncurry constructFuncInfo <$> [("sort", sort), ("reverse", reverse)])

-- | The `FuncInfo` of the function that indexes into a list.
funcInfoIndex :: FuncInfo m
funcInfoIndex = FuncInfo "index" [ATInteger, ATIntegerList] ATInteger fiIndex
  where
    fiIndex (LIInteger i : [LIList is])
      | i < 0 || i >= genericLength is = throwBot $ EvaluationException ("index out of range: " ++ show i) []
      | otherwise = return (is !! fromInteger i)
    fiIndex is = throwBot $ EvaluationException ("incorrect number of arguments. expected 2, got " ++ show (length is)) []

-- | A data structure to contain the information about a given function,
-- including types, the function name, and the function itself.
data FuncInfoBase m j = FuncInfo {funcInfoName :: Text, funcInfoParameters :: [ArgType], funcReturnType :: ArgType, funcInfoFunc :: MonadException m => [ListInteger] -> m j}

type FuncInfo m = FuncInfoBase m Integer

instance Show (FuncInfoBase m j) where
  show (FuncInfo fin ft frt _) = "FuncInfo " <> unpack fin <> " " <> show ft <> " " <> show frt

-- | A simple way to construct a function that returns a value j, and has no
-- constraints on the given values.
constructFuncInfo :: forall j f m. (MonadException m, ApplyFunc m f, Returns f ~ j) => Text -> f -> FuncInfoBase m j
constructFuncInfo s f = constructFuncInfo' s f (Nothing, Nothing, const False)

-- | Construct a function info when given optional constraints.
constructFuncInfo' :: forall j f m. (MonadException m, ApplyFunc m f, Returns f ~ j) => Text -> f -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> FuncInfoBase m j
constructFuncInfo' s f bs = FuncInfo s params (last types) (applyFunc f (fromIntegral (length params)) bs)
  where
    types = getTypes f
    params = init types

-- | Some evaluated values, either an integer or a list of values with their
-- representations.
data ListInteger = LIInteger Integer | LIList [Integer]
  deriving (Show, Eq, Ord)

-- | Values representing argument types.
data ArgType = ATInteger | ATIntegerList
  deriving (Show, Eq)

-- | A type class for counting the amount of arguments of a function, and their
-- types. Only supports integers and integer lists currently.
class ArgCount f where
  -- | Get the number of arguments to a function.
  getArgs :: f -> Integer
  getArgs = (+ (-1)) . fromIntegral . length . getTypes

  -- | Get the types of arguments to a function.
  getTypes :: f -> [ArgType]

instance ArgCount Integer where
  getTypes _ = [ATInteger]

instance ArgCount [Integer] where
  getTypes _ = [ATIntegerList]

instance ArgCount f => ArgCount (Integer -> f) where
  getTypes f = ATInteger : getTypes (f 1)

instance ArgCount f => ArgCount ([Integer] -> f) where
  getTypes f = ATIntegerList : getTypes (f [1])

-- | Type class which represents applying a function f to some inputs when given
-- the bounds for the function and some number of inputs.
--
-- If the number of inputs is incorrect or the value given out of the range, an
-- exception is thrown.
class ArgCount f => ApplyFunc m f where
  -- | Takes a function, the number of arguments in the function overall, bounds
  -- on integer values to the function, and a list of `ListInteger`s (which are
  -- either a list of integers or an integer), and returns a wrapped `j` value,
  -- which is a value that the function originally returns.
  applyFunc :: (MonadException m, Returns f ~ j) => f -> Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> [ListInteger] -> m j

-- | Check whether a given value is within the given bounds.
checkBounds :: (MonadException m) => Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> m Integer
checkBounds i (ml, mh, bs)
  | not (maybe True (i >) ml) = throwBot $ EvaluationException ("value too low for function. expected >" <> show (fromJust ml) <> ", got " <> show i) []
  | not (maybe True (i <) mh) = throwBot $ EvaluationException ("value too high for function. expected <" <> show (fromJust mh) <> ", got " <> show i) []
  | bs i = throwBot $ EvaluationException ("invalid value for function: `" <> show i ++ "`") []
  | otherwise = return i

instance {-# OVERLAPPING #-} ApplyFunc m Integer where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

instance {-# OVERLAPPING #-} ApplyFunc m [Integer] where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

instance {-# OVERLAPPABLE #-} (ApplyFunc m f) => ApplyFunc m (Integer -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIInteger x) : xs) = checkBounds x bs >>= \x' -> applyFunc (f x') args bs xs
  applyFunc _ _ _ (_ : _) = throwBot $ EvaluationException "incorrect type given to function. expected an integer, got a list" []

instance {-# OVERLAPPABLE #-} (ApplyFunc m f) => ApplyFunc m ([Integer] -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIList x) : xs) = applyFunc (f x) args bs xs
  applyFunc _ _ _ (_ : _) = throwBot $ EvaluationException "incorrect type given to function. expected a list, got an integer" []

-- | Simple type family that gets the return type of whatever function or value
-- is given
type family Returns f where
  Returns (i -> j) = Returns j
  Returns j = j
