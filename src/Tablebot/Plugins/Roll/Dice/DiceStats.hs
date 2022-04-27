-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given
-- expressions.
module Tablebot.Plugins.Roll.Dice.DiceStats (rangeExpr, rangeListValues, getStats) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Distribution as D
import Data.List
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase
import Tablebot.Utility.Exception (catchBot)

class (MonadException m, MonadExperiment m) => MonadExEx m

instance (MonadException m, MonadExperiment m) => MonadExEx m

-- | Alias for an experiment of integers.
--
-- Where a distribution is a concrete mapping between values and probabilities,
-- an Experiment is more a monadic representation of a Distribution, effectively
-- deferring calculation to the end.
--
-- I'm not sure if it's more efficient but it certainly makes composing things
-- a lot easier
-- type Experiment = D.Experiment Integer

-- | Convenient alias for a experiments of lists of integers.
-- type ExperimentList = D.Experiment [Integer]

-- | Lifed form of `D.Experiment`, so it can handle exceptions.
nullExperiment :: MonadExperiment m => m (Identity Integer)
nullExperiment = Identity <$> from nullDistribution

nullExperimentList :: MonadExperiment m => m [Integer]
nullExperimentList = (: []) <$> from nullDistribution

-- | Get the most common values, the mean, and the standard deviation of a given
-- distribution.
getStats :: Distribution -> ([Integer], Double, Double)
getStats d = (modalOrder, D.expectation d, D.standardDeviation d)
  where
    vals = D.toList d
    modalOrder = fst <$> sortBy (\(_, r) (_, r') -> compare r' r) vals

-- -- | Convenience wrapper which gets the range of the given values then applies
-- -- the function to the resultant distributions.
-- combineRangesBinOp :: (MonadException m, Range Identity a, Range Identity b, PrettyShow a, PrettyShow b) => (Integer -> Integer -> Integer) -> a -> b -> m Integer
-- combineRangesBinOp f a b = do
--   d <- range a
--   d' <- range b
--   return $ f <$> d <*> d'

rangeExpr :: (MonadException m) => Expr -> m Distribution
rangeExpr e = do
  let r :: ExceptionT D.Experiment Integer = runIdentity <$> range @Identity e
      r' :: D.Experiment (Either SomeException Integer) = runExceptionT @D.Experiment r
      r'' :: ExceptionT D.Experiment (D.Distribution Integer) = run r
  -- r''' = run r' -- Could not deduce (Ord SomeException) arising from a use of ‘run’

  d <- runExceptionT $ run r
  e' <- (runIdentity <$> range @Identity e)
  return

-- return $ D.run $ runIdentity <$> e'

rangeListValues :: (MonadException m) => ListValues -> m [Distribution]
rangeListValues lv = do
  lve <- mapM liftException $ runExceptionT (range @[] @_ @(ExceptionT D.Experiment) lv)
  let lvd = D.toList $ D.run lve
  return $ D.fromList <$> zip' lvd
  where
    head' [] = []
    head' (x : _) = [x]
    getHeads xs = (\(xs', p) -> (,p) <$> head' xs') =<< xs
    getTails xs = first tail <$> xs
    zip' xs = getHeads xs : zip' (getTails xs)

-- | Type class to get the overall range of a value.
--
-- A `Data.Distribution.Distribution` is a map of values to probabilities, and
-- has a variety of  functions that operate on them.
--
-- An `Data.Distribution.Experiment` is a monadic form of this.
class PrettyShow a => Range f a where
  -- | Try and get the `Experiment` of the given value, throwing a
  -- `MonadException` on failure.
  range :: (PrettyShow a, MonadExEx m) => a -> m (f Integer)
  range a = propagateException (prettyShow a) (range' a)

  range' :: (PrettyShow a, MonadExEx m) => a -> m (f Integer)

-- instance (Range a) => Range Identity (MiscData a) where
--   range' (MiscVar l) = range l
--   range' (MiscIf i) = rangeIfExpr range i

instance (Range f a) => Range f (MiscData a) where
  range' (MiscVar l) = range l
  range' (MiscIf i) = rangeIfExpr range i

rangeIfExpr :: (MonadExEx m) => (a -> m b) -> If a -> m b
rangeIfExpr func (If b t f) = do
  b' <- range @Identity b
  if b' /= 0 then func t else func f

-- rangeIfList :: (MonadException m, Ord b) => (a -> m (D.Experiment b)) -> If ListValues a -> m (D.Experiment b)
-- rangeIfList func (If b t f) = do
--   b' <- rangeList b
--   let mp = toMap $ run b'
--       canBeFalse = M.member [] mp
--       canBeTrue = M.null $ M.filterWithKey (\k _ -> k /= []) mp
--       emptyExp = from $ D.fromList @_ @Integer []
--   t' <- if canBeTrue then func t else return emptyExp
--   f' <- if canBeFalse then func f else return emptyExp
--   return $
--     do
--       b'' <- b'
--       if b'' /= [] then t' else f'

instance (Range f a) => Range f (Var a) where
  range' (Var _ a) = range a
  range' (VarLazy _ a) = range a

instance Range Identity Expr where
  range' (NoExpr t) = range t
  range' (Add t e) = (+) <$> range t <*> range e
  range' (Sub t e) = (-) <$> range t <*> range e
  range' (ExprMisc t) = range t

instance Range Identity Term where
  range' (NoTerm t) = range t
  range' (Multi t e) = (*) <$> range t <*> range e
  range' (Div t e) = do
    d <- range t
    d' <- range e
    -- If 0 is always the denominator, the distribution will be empty.
    if d' == 0 then nullExperiment else return $ d `div` d'

instance Range Identity Negation where
  range' (Neg t) = fmap negate <$> range t
  range' (NoNeg t) = range t

instance Range Identity Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range @Identity e
    -- if the exponent is always negative, the distribution will be empty
    if d' < 0 then nullExperiment else return $ d ^ d'

instance Range Identity Func where
  range' (NoFunc t) = range t
  range' (Func fi avs) = Identity <$> rangeFunction (runIdentity <$> nullExperiment) fi avs

instance Range Identity NumBase where
  range' (Value i) = return $ return i
  range' (NBParen (Paren e)) = range e

instance Range Identity Base where
  range' (NBase nb) = range nb
  range' (DiceBase d) = range d
  range' b@(NumVar _) = evaluationException "cannot find range of variable" [prettyShow b]

instance Range Identity Die where
  range' (LazyDie d) = range d
  range' (Die nb) = do
    nbr <- range nb
    from (D.uniform [1 .. nbr])
  range' (CustomDie lv) = do
    dievs <- range @[] lv
    Identity <$> from (D.uniform dievs)

instance Range Identity Dice where
  range' (Dice b d mdor) = do
    b' <- range @Identity b
    -- d' <- range @Identity d
    let d' = runIdentity <$> range @Identity d
        e = getDiceExperiment b' =<< run d'
    res <- rangeDiceExperiment d' mdor e
    return $ Identity $ sum res

-- | Get the distribution of values from a given number of (identically
-- distributed) values and the distribution of that value.
getDiceExperiment :: MonadExperiment m => Integral i => i -> Distribution -> m [Integer]
getDiceExperiment i = replicateM (fromIntegral i) . from

-- | Go through each operator on dice and modify the `Experiment` representing
-- all possible collections of rolls, returning the `Experiment` produced on
-- finding `Nothing`.
rangeDiceExperiment :: MonadExEx m => m Integer -> Maybe DieOpRecur -> m [Integer] -> m [Integer]
rangeDiceExperiment _ Nothing is = is
rangeDiceExperiment die (Just (DieOpRecur doo mdor)) is = rangeDiceExperiment die mdor (rangeDieOpExperiment die doo is)

-- | Perform one dice operation on the given `Experiment`, possibly returning
-- a modified experiment representing the distribution of dice rolls.
rangeDieOpExperiment :: MonadExEx m => m Integer -> DieOpOption -> m [Integer] -> m [Integer]
rangeDieOpExperiment die (DieOpOptionLazy o) is = rangeDieOpExperiment die o is
rangeDieOpExperiment _ (DieOpOptionKD kd lhw) is = rangeDieOpExperimentKD kd lhw is
rangeDieOpExperiment die (Reroll rro cond lim) is = do
  limit <- runIdentity <$> range lim
  let newDie = mkNewDie limit
  rolls <- is
  let (count, cutdownRolls) = countTriggers limit rolls
  if count == 0
    then return cutdownRolls
    else (cutdownRolls ++) <$> (getDiceExperiment count =<< run newDie)
  where
    mkNewDie limitValue
      | rro = die
      | otherwise = (from . D.assuming (\i -> not $ applyCompare cond i limitValue)) =<< run die
    countTriggers limitValue = foldr (\i (c, xs') -> if applyCompare cond i limitValue then (c + 1, xs') else (c, i : xs')) (0, [])

-- | Perform a keep/drop operation on the `Experiment` of dice rolls.
rangeDieOpExperimentKD :: MonadExEx m => KeepDrop -> LowHighWhere -> m [Integer] -> m [Integer]
rangeDieOpExperimentKD kd (Where cond nb) is = do
  wherelimit <- runIdentity <$> range nb
  filter (\i -> keepDrop $ applyCompare cond i wherelimit) <$> is
  where
    keepDrop
      | kd == Keep = id
      | otherwise = not
rangeDieOpExperimentKD kd lhw is = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      kdlh <- range @Identity nb'
      getKeep kdlh . sortBy' <$> is
  where
    -- the below exception should never trigger - it is a hold over. it is
    -- present so that this thing type checks nicely.
    whereException = evaluationException "keep/drop where is unsupported" []
    order l l' = if isLow lhw then compare l l' else compare l' l
    sortBy' = sortBy order
    getKeep = if kd == Keep then genericTake else genericDrop

-- -- | Type class to get the overall range of a list of values.
-- --
-- -- Only used within `DiceStats` as I have no interest in producing statistics on
-- -- lists
-- class PrettyShow a => RangeList a where
--   -- | Try and get the `DistributionList` of the given value, throwing a
--   -- `MonadException` on failure.
--   rangeList :: (MonadException m, PrettyShow a) => a -> m []
--   rangeList a = propagateException (prettyShow a) (rangeList' a)

--   rangeList' :: (MonadException m, PrettyShow a) => a -> m ExperimentList

instance Range [] ListValuesBase where
  range' (LVBList es) = (runIdentity <$>) <$> mapM (range @Identity) es
  range' (LVBParen (Paren lv)) = range lv

instance Range [] ListValues where
  range' (LVBase lvb) = range lvb
  range' (MultipleValues nb b) = do
    nbd <- range @Identity nb
    bd <- run $ runIdentity <$> range @Identity b
    getDiceExperiment nbd bd
  range' (LVFunc fi avs) = rangeFunction nullExperimentList fi avs
  range' (ListValuesMisc m) = range m
  range' b@(LVVar _) = evaluationException "cannot find range of variable" [prettyShow b]

rangeArgValue :: MonadExEx m => ArgValue -> m (Identity ListInteger)
rangeArgValue (AVExpr e) = (LIInteger <$>) <$> range @Identity e
rangeArgValue (AVListValues lv) = Identity . LIList <$> range @[] lv

rangeFunction :: forall j m. (Ord j, MonadExEx m) => m j -> FuncInfoBase j -> [ArgValue] -> m j
rangeFunction null' fi exprs = do
  exprs' <- mapM rangeArgValue exprs
  let params :: m j = funcInfoFunc fi (runIdentity <$> exprs')
  catchBot params (const null')

-- from . D.fromList <$> foldAndIgnoreErrors params
-- where
--   foldAndIgnoreErrors = foldr (\(mv, p) mb -> mb >>= \b -> catchBot ((: []) . (,p) <$> mv) (const (return [])) >>= \v -> return (v ++ b)) (return [])
