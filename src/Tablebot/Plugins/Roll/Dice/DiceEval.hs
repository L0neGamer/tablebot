-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceEval
-- Description : How to evaluate dice and expressions
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions, type classes, and other utilities to evaluate dice values and
-- expressions.
module Tablebot.Plugins.Roll.Dice.DiceEval (PrettyShow (prettyShow), evalProgram, evalList, evalInteger, evaluationException, propagateException, maximumRNG, maximumListLength) where

import Control.Monad.Exception (MonadException)
import Control.Monad.Random.Class (MonadRandom (getRandomR))
import Control.Monad.State (StateT, evalStateT, gets, modify, when)
import Data.List (foldl', genericDrop, genericReplicate, genericTake, sortBy)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, (<|))
import Data.Map (Map, empty)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text, intercalate, pack, unpack)
import qualified Data.Text as T
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceFunctions (FuncInfoBase (..), ListInteger (..))
import Tablebot.Utility.Discord (Format (..), formatInput, formatText)
import Tablebot.Utility.Exception (BotException (EvaluationException), catchBot, throwBot)
import Tablebot.Utility.Random (chooseOne)

-- | A wrapper type to differentiate between the RNGCount and other Integers,
-- as well as store variables throughout the program.
--
-- Represents the total number of calls to the RNG throughout the program
-- (effectively, how many die rolls have occured).
data ProgramState = ProgramState
  { getRNGCount :: Integer,
    getVariables :: Map Text (Either ListValues Expr)
  }
  deriving (Show)

startState :: ProgramState
startState = ProgramState 0 empty

-- | Minor typeclass to make it easier to get both random and exception
-- functionality.
class (MonadRandom m, MonadException m) => MonadRandomException m

instance (MonadRandom m, MonadException m) => MonadRandomException m

type ProgramStateM m = StateT ProgramState m

-- | Add the given variable to the `ProgramState`.
addVariable :: MonadRandom m => Text -> Either ListValues Expr -> ProgramStateM m ()
addVariable t val = modify $ \s -> s {getVariables = M.insert t val (getVariables s)}

-- | The maximum depth that should be permitted. Used to limit number of dice
-- and rerolls.
maximumRNG :: Integer
maximumRNG = 150

maximumListLength :: Integer
maximumListLength = 50

-- | Increment the rngcount by 1.
incRNGCount :: MonadException m => ProgramStateM m ()
incRNGCount = modify (\s -> s {getRNGCount = getRNGCount s + 1}) >> checkRNGCount

-- | Check whether the RNG count has been exceeded by the integer given.
checkRNGCount :: MonadException m => ProgramStateM m ()
checkRNGCount = do
  rngCount <- gets getRNGCount
  when (rngCount > maximumRNG) $ evaluationException ("Maximum RNG count exceeded (" <> pack (show maximumRNG) <> ")") []

-- | Utility function to throw an `EvaluationException` when using `Text`.
evaluationException :: MonadException m => Text -> [Text] -> m a
evaluationException nm locs = throwBot $ EvaluationException (unpack nm) (unpack <$> locs)

--- Evaluating an expression.

-- | Evaluating a full program.
evalProgram :: MonadRandomException m => Program -> m (Either [(Integer, Text)] Integer, Text)
evalProgram (Program ss elve) =
  evalStateT
    ( do
        t <- foldl' (\b s -> b >>= \t -> evalStatement s >>= \st -> return (t <> st)) (return "") ss
        r <- either ((Left <$>) . evalShowL) ((Right <$>) . evalShow) elve
        case r of
          Left (is, mt) -> return (Left is, t <> fromMaybe (prettyShow elve) mt)
          Right (is, mt) -> return (Right is, t <> mt)
    )
    startState

-- | Given a list expression, evaluate it, getting the pretty printed string and
-- the value of the result.
evalList :: (EvalList a, PrettyShow a, MonadRandomException m) => a -> m ([(Integer, Text)], Text)
evalList a = do
  (is, ss) <- evalStateT (evalShowL a) startState
  return (is, fromMaybe (prettyShow a) ss)

-- | Given an integer expression, evaluate it, getting the pretty printed string
-- and the value of the result.
evalInteger :: (Eval a, PrettyShow a, MonadRandomException m) => a -> m (Integer, Text)
evalInteger a = do
  (is, ss) <- evalStateT (evalShow a) startState
  return (is, ss)

-- | Utility function to display dice.
--
-- The tuple of integers denotes what the critvalues of this dice value are. The
-- `a` denotes the value that is being printed, and needs to have `PrettyShow`
-- defined for it.
--
-- Finally, the list of tuples denotes all the values that the `a` value has
-- gone through. If the `Maybe Bool` value is `Nothing`, the number is displayed
-- as normal. If the value is `Just False`, the value has been rerolled over,
-- and is displayed crossed out. If the value is `Just True`, the value has been
-- dropped, and the number is crossed out and underlined.
dieShow :: (PrettyShow a, MonadRandomException m) => Maybe (Integer, Integer) -> a -> [(Integer, Maybe Bool)] -> m Text
dieShow _ a [] = evaluationException "tried to show empty set of results" [prettyShow a]
dieShow lchc d ls = return $ prettyShow d <> " [" <> intercalate ", " adjustList <> "]"
  where
    toCrit =
      pack
        . if isNothing lchc
          then show
          else toCrit'
    (lc, hc) = fromMaybe (0, 0) lchc
    toCrit' i
      | i == lc || i == hc = formatInput Bold i
      | otherwise = show i
    toCrossedOut (i, Just False) = formatText Strikethrough $ toCrit i
    toCrossedOut (i, Just True) = formatText Strikethrough $ formatText Underline $ toCrit i
    toCrossedOut (i, _) = toCrit i
    adjustList = fmap toCrossedOut ls

-- | Evaluate a series of values, combining the text output into a comma
-- separated list.
evalShowList :: (Eval a, PrettyShow a, MonadRandomException m) => [a] -> ProgramStateM m ([Integer], Text)
evalShowList as = do
  vs <- evalShowList' as
  let (is, ts) = unzip vs
  return (is, intercalate ", " ts)

-- | Evaluate a series of values, combining the text output a list.
evalShowList' :: (Eval a, PrettyShow a, MonadRandomException m) => [a] -> ProgramStateM m [(Integer, Text)]
evalShowList' = evalShowList'' evalShow

-- | Evaluate (using a custom evaluator function) a series of values, getting
-- strings and values as a result.
evalShowList'' :: (MonadRandomException m) => (a -> ProgramStateM m (i, Text)) -> [a] -> ProgramStateM m [(i, Text)]
evalShowList'' customEvalShow as = foldl' (flip foldF) (return []) as >>= \lst -> return (reverse lst)
  where
    foldF a sumrngcount = do
      diceSoFar <- sumrngcount
      (i, s) <- customEvalShow a
      return ((i, s) : diceSoFar)

-- | When given a value that may possibly have an `EvaluationException`, add the
-- representation of the current value to the exception stack.
propagateException :: (MonadException m) => Text -> m v -> m v
propagateException t a = catchBot a handleException
  where
    handleException (EvaluationException msg' locs) = throwBot (EvaluationException msg' (addIfNotIn locs))
    handleException e = throwBot e
    pa = unpack t
    addIfNotIn locs = if null locs || pa /= Prelude.head locs then pa : locs else locs

-- | This type class evaluates an item and returns a list of integers (with
-- their representations if valid).
class EvalList a where
  -- | Evaluate the given item into a list of integers and text,
  -- possibly a string representation of the value, and the number of RNG calls
  -- it took. If the `a` value is a dice value, the values of the dice should be
  -- displayed. This function adds the current location to the exception
  -- callstack.
  evalShowL :: (PrettyShow a, MonadRandomException m) => a -> ProgramStateM m ([(Integer, Text)], Maybe Text)
  evalShowL a = do
    (is, mt) <- propagateException (prettyShow a) (evalShowL' a)
    return (genericTake maximumListLength is, mt)

  evalShowL' :: (PrettyShow a, MonadRandomException m) => a -> ProgramStateM m ([(Integer, Text)], Maybe Text)

evalArgValue :: (MonadRandomException m) => ArgValue -> ProgramStateM m ListInteger
evalArgValue (AVExpr e) = do
  (i, _) <- evalShow e
  return $ LIInteger i
evalArgValue (AVListValues e) = do
  (i, _) <- evalShowL e
  return (LIList (fst <$> i))

instance EvalList ListValues where
  evalShowL' (MultipleValues nb b) = do
    (nb', _) <- evalShow nb
    vs <- evalShowList' (genericReplicate nb' b)
    return (vs, Nothing)
  evalShowL' (LVFunc fi exprs) = evaluateFunction fi exprs >>= \(i, s) -> return ((,"") <$> i, Just s)
  evalShowL' (LVBase lvb) = evalShowL lvb
  evalShowL' (LVVar t) = do
    vars <- gets getVariables
    case M.lookup t vars of
      Just (Left e) -> evalShowL e >>= \(i, _) -> return (i, Just t)
      _ -> evaluationException ("could not find list variable `" <> t <> "`") []
  evalShowL' (ListValuesMisc l) = evalShowL l

instance EvalList ListValuesBase where
  evalShowL' (LVBList es) = do
    vs <- evalShowList' es
    return (vs, Nothing)
  evalShowL' (LVBParen (Paren lv)) = evalShowL lv

instance EvalList ListValuesMisc where
  evalShowL' (MiscVar l) = evalShowL l
  evalShowL' (MiscIf l) = evalShowL l

-- | This type class gives a function which evaluates the value to an integer
-- and a string.
class Eval a where
  -- | Evaluate the given item to an integer, a string representation of the
  -- value, and the number of RNG calls it took. If the `a` value is a dice
  -- value, the values of the dice should be displayed. This function adds
  -- the current location to the exception callstack.
  evalShow :: (PrettyShow a, MonadRandomException m) => a -> ProgramStateM m (Integer, Text)
  evalShow a = propagateException (prettyShow a) (evalShow' a)

  evalShow' :: (PrettyShow a, MonadRandomException m) => a -> ProgramStateM m (Integer, Text)

instance Eval Base where
  evalShow' (NBase nb) = evalShow nb
  evalShow' (DiceBase dice) = evalShow dice
  evalShow' (NumVar t) = do
    vars <- gets getVariables
    case M.lookup t vars of
      Just (Right e) -> evalShow e >>= \(i, _) -> return (i, t)
      _ -> evaluationException ("could not find integer variable `" <> t <> "`") []

instance Eval Die where
  evalShow' ld@(LazyDie d) = do
    (i, _) <- evalShow d
    ds <- dieShow Nothing ld [(i, Nothing)]
    return (i, ds)
  evalShow' d@(CustomDie (LVBList es)) = do
    e <- chooseOne es
    (i, _) <- evalShow e
    ds <- dieShow Nothing d [(i, Nothing)]
    incRNGCount
    return (i, ds)
  evalShow' d@(CustomDie is) = do
    (is', _) <- evalShowL is
    i <- chooseOne (fst <$> is')
    ds <- dieShow Nothing d [(i, Nothing)]
    incRNGCount
    return (i, ds)
  evalShow' d@(Die b) = do
    (bound, _) <- evalShow b
    if bound < 1
      then evaluationException ("Cannot roll a < 1 sided die (" <> formatText Code (prettyShow b) <> ")") []
      else do
        i <- getRandomR (1, bound)
        ds <- dieShow Nothing d [(i, Nothing)]
        incRNGCount
        return (i, ds)

instance Eval Dice where
  evalShow' dop = do
    (lst, mnmx) <- evalDieOp dop
    let vs = fromEvalDieOpList lst
    s <- dieShow mnmx dop vs
    return (sum (fst <$> filter (isNothing . snd) vs), s)

-- | Utility function to transform the output list type of other utility
-- functions into one that `dieShow` recognises.
fromEvalDieOpList :: [(NonEmpty Integer, Bool)] -> [(Integer, Maybe Bool)]
fromEvalDieOpList = foldr foldF []
  where
    foldF (is, b) lst = let is' = (,Just False) <$> NE.tail is in (reverse ((NE.head is, if b then Nothing else Just True) : is') <> lst)

-- | Helper function that takes a set of Dice and returns a tuple of three
-- items. The second item is the base die. The values returns are: a list of all
-- the dice rolled, the history of rerolls, and whether the die was dropped;
-- the range of the die (if applicable), and the amount of RNG calls made.
--
-- The function itself checks to make sure the number of dice being rolled is
-- less than the maximum recursion and is non-negative.
evalDieOp :: MonadRandomException m => Dice -> ProgramStateM m ([(NonEmpty Integer, Bool)], Maybe (Integer, Integer))
evalDieOp (Dice b ds dopo) = do
  (nbDice, _) <- evalShow b
  if nbDice > maximumRNG
    then evaluationException ("tried to roll more than " <> formatInput Code maximumRNG <> " dice: " <> formatInput Code nbDice) [prettyShow b]
    else do
      if nbDice < 0
        then evaluationException ("tried to give a negative value to the number of dice: " <> formatInput Code nbDice) [prettyShow b]
        else do
          (ds', crits) <- condenseDie ds
          (rolls, _) <- evalShowList (genericReplicate nbDice ds')
          let vs = fmap (\i -> (i :| [], True)) rolls
          rs <- evalDieOp' dopo ds' vs
          return (sortBy sortByOption rs, crits)
  where
    condenseDie (Die dBase) = do
      (i, _) <- evalShow dBase
      return (Die (Value i), Just (1, i))
    condenseDie (CustomDie is) = do
      (is', _) <- evalShowL is
      return (CustomDie (LVBList (promote . fst <$> is')), Nothing)
    condenseDie (LazyDie d) = return (d, Nothing)
    sortByOption (e :| es, _) (f :| fs, _)
      | e == f = compare (length fs) (length es)
      | otherwise = compare e f

-- | Utility function that processes a `Maybe DieOpRecur`, when given a die, and
-- dice that have already been processed.
evalDieOp' :: MonadRandomException m => Maybe DieOpRecur -> Die -> [(NonEmpty Integer, Bool)] -> ProgramStateM m [(NonEmpty Integer, Bool)]
evalDieOp' Nothing _ is = return is
evalDieOp' (Just (DieOpRecur doo mdor)) die is = do
  doo' <- processDOO doo
  is' <- evalDieOp'' doo' die is
  evalDieOp' mdor die is'
  where
    processLHW (Low i) = do
      (i', _) <- evalShow i
      return (Low (Value i'))
    processLHW (High i) = do
      (i', _) <- evalShow i
      return (High (Value i'))
    processLHW (Where o i) = do
      (i', _) <- evalShow i
      return (Where o (Value i'))
    processDOO (DieOpOptionKD kd lhw) = do
      lhw' <- processLHW lhw
      return (DieOpOptionKD kd lhw')
    processDOO (Reroll once o i) = do
      (i', _) <- evalShow i
      return (Reroll once o (Value i'))
    processDOO (DieOpOptionLazy doo') = return doo'

-- | Utility function that processes a `DieOpOption`, when given a die, and dice
-- that have already been processed.
evalDieOp'' :: MonadRandomException m => DieOpOption -> Die -> [(NonEmpty Integer, Bool)] -> ProgramStateM m [(NonEmpty Integer, Bool)]
evalDieOp'' (DieOpOptionLazy doo) die is = evalDieOp'' doo die is
evalDieOp'' (DieOpOptionKD kd lhw) _ is = evalDieOpHelpKD kd lhw is
evalDieOp'' (Reroll once o i) die is = foldr rerollF (return []) is
  where
    rerollF g@(i', b) isRngCount' = do
      is' <- isRngCount'
      (iEval, _) <- evalShow i
      if b && applyCompare o (NE.head i') iEval
        then do
          (v, _) <- evalShow die
          let ret = (v <| i', b)
          if once
            then return (ret : is')
            else rerollF ret (return is')
        else return (g : is')

-- | Given a list of dice values, separate them into kept values and dropped values
-- respectively.
separateKeptDropped :: [(NonEmpty Integer, Bool)] -> ([(NonEmpty Integer, Bool)], [(NonEmpty Integer, Bool)])
separateKeptDropped = foldr f ([], [])
  where
    f a@(_, True) (kept, dropped) = (a : kept, dropped)
    f a@(_, False) (kept, dropped) = (kept, a : dropped)

-- | Utility function to set all the values in the given list to be dropped.
setToDropped :: [(NonEmpty Integer, Bool)] -> [(NonEmpty Integer, Bool)]
setToDropped = fmap (\(is, _) -> (is, False))

-- | Helper function that executes the keep/drop commands on dice.
evalDieOpHelpKD :: MonadRandomException m => KeepDrop -> LowHighWhere -> [(NonEmpty Integer, Bool)] -> ProgramStateM m [(NonEmpty Integer, Bool)]
evalDieOpHelpKD kd (Where cmp i) is = foldr foldF (return []) is
  where
    isKeep = if kd == Keep then id else not
    foldF (iis, b) sumrngcount = do
      diceSoFar <- sumrngcount
      (i', _) <- evalShow i
      return ((iis, b && isKeep (applyCompare cmp (NE.head iis) i')) : diceSoFar)
evalDieOpHelpKD kd lh is = do
  (i', _) <- evalShow i
  return (d <> setToDropped (getDrop i' sk) <> getKeep i' sk)
  where
    (k, d) = separateKeptDropped is
    -- Note that lh will always be one of `Low` or `High`
    order l l' = if isLow lh then compare l l' else compare l' l
    sk = sortBy order k
    i = fromMaybe (Value 0) (getValueLowHigh lh)
    (getDrop, getKeep) = if kd == Keep then (genericDrop, genericTake) else (genericTake, genericDrop)

--- Pure evaluation functions for non-dice calculations
-- Was previously its own type class that wouldn't work for evaluating Base values.

-- | Utility function to evaluate a binary operator.
binOpHelp :: (Eval a, Eval b, PrettyShow a, PrettyShow b, MonadRandomException m) => a -> b -> Text -> (Integer -> Integer -> Integer) -> ProgramStateM m (Integer, Text)
binOpHelp a b opS op = do
  (a', a's) <- evalShow a
  (b', b's) <- evalShow b
  return (op a' b', a's <> " " <> opS <> " " <> b's)

instance Eval ExprMisc where
  evalShow' (MiscVar l) = evalShow l
  evalShow' (MiscIf l) = evalShow l

instance Eval Expr where
  evalShow' (NoExpr t) = evalShow t
  evalShow' (ExprMisc e) = evalShow e
  evalShow' (Add t e) = binOpHelp t e "+" (+)
  evalShow' (Sub t e) = binOpHelp t e "-" (-)

instance Eval Term where
  evalShow' (NoTerm f) = evalShow f
  evalShow' (Multi f t) = binOpHelp f t "*" (*)
  evalShow' (Div f t) = do
    (f', f's) <- evalShow f
    (t', t's) <- evalShow t
    if t' == 0
      then evaluationException "division by zero" [prettyShow t]
      else return (div f' t', f's <> " / " <> t's)

instance Eval Func where
  evalShow' (Func s exprs) = evaluateFunction s exprs
  evalShow' (NoFunc b) = evalShow b

-- | Evaluate a function when given a list of parameters
evaluateFunction :: MonadRandomException m => FuncInfoBase j -> [ArgValue] -> ProgramStateM m (j, Text)
evaluateFunction fi exprs = do
  exprs' <- evalShowList'' (fmap (,"") . evalArgValue) exprs
  f <- funcInfoFunc fi (fst <$> exprs')
  return (f, funcInfoName fi <> "(" <> intercalate ", " (prettyShow <$> exprs) <> ")")

instance Eval Negation where
  evalShow' (NoNeg expo) = evalShow expo
  evalShow' (Neg expo) = do
    (expo', expo's) <- evalShow expo
    return (negate expo', "-" <> expo's)

instance Eval Expo where
  evalShow' (NoExpo b) = evalShow b
  evalShow' (Expo b expo) = do
    (expo', expo's) <- evalShow expo
    if expo' < 0
      then evaluationException ("the exponent is negative: " <> formatInput Code expo') [prettyShow expo]
      else do
        (b', b's) <- evalShow b
        return (b' ^ expo', b's <> " ^ " <> expo's)

instance Eval NumBase where
  evalShow' (NBParen (Paren e)) = do
    (r, s) <- evalShow e
    return (r, "(" <> s <> ")")
  evalShow' (Value i) = return (i, pack (show i))

instance Eval (Var Expr) where
  evalShow' (Var t a) = do
    (v, lt) <- evalShow a
    addVariable t (Right $ promote v)
    return (v, "var " <> t <> " = " <> lt)
  evalShow' l@(VarLazy t a) = do
    (v, _) <- evalShow a
    addVariable t (Right a)
    return $ v `seq` (v, prettyShow l)

instance EvalList (Var ListValues) where
  evalShowL' l@(Var t a) = do
    (v, _) <- evalShowL a
    addVariable t (Left $ promote $ fst <$> v)
    return (v, Just (prettyShow l))
  evalShowL' l@(VarLazy t a) = do
    (v, _) <- evalShowL a
    addVariable t (Left a)
    return (v, Just (prettyShow l))

evalStatement :: MonadRandomException m => Statement -> ProgramStateM m Text
evalStatement (StatementExpr l) = evalShowStatement l >>= \(_, t) -> return (t <> "; ")
  where
    evalShowStatement (ExprMisc (MiscVar l'@(VarLazy t a))) = addVariable t (Right a) >> return (0, prettyShow l')
    evalShowStatement l' = evalShow l'
evalStatement (StatementListValues l) = evalShowStatement l >>= \(_, t) -> return (fromMaybe (prettyShow l) t <> "; ")
  where
    evalShowStatement (ListValuesMisc (MiscVar l'@(VarLazy t a))) = addVariable t (Left a) >> return ([], Just (prettyShow l'))
    evalShowStatement l' = evalShowL l'

instance Eval (If Expr) where
  evalShow' if'@(If b t e) = do
    (i, _) <- evalShow b
    (i', _) <-
      if i /= 0
        then evalShow t
        else evalShow e
    return (i', prettyShow if')

instance EvalList (If ListValues) where
  evalShowL' if'@(If b t e) = do
    (i, _) <- evalShow b
    (i', _) <-
      if i /= 0
        then evalShowL t
        else evalShowL e
    return (i', Just $ prettyShow if')

--- Pretty printing the AST
-- The output from this should be parseable

-- | Type class to display an expression prettily (not neccessarily accurately).
class PrettyShow a where
  -- | Print the given value prettily.
  prettyShow :: a -> Text

instance PrettyShow ArgValue where
  prettyShow (AVExpr e) = prettyShow e
  prettyShow (AVListValues lv) = prettyShow lv

instance PrettyShow ListValues where
  prettyShow (LVBase e) = prettyShow e
  prettyShow (MultipleValues nb b) = prettyShow nb <> "#" <> prettyShow b
  prettyShow (LVFunc s n) = funcInfoName s <> "(" <> intercalate "," (prettyShow <$> n) <> ")"
  prettyShow (LVVar t) = t
  prettyShow (ListValuesMisc l) = prettyShow l

instance PrettyShow ListValuesBase where
  prettyShow (LVBList es) = "{" <> intercalate ", " (prettyShow <$> es) <> "}"
  prettyShow (LVBParen p) = prettyShow p

instance PrettyShow a => PrettyShow (MiscData a) where
  prettyShow (MiscVar l) = prettyShow l
  prettyShow (MiscIf l) = prettyShow l

instance PrettyShow Expr where
  prettyShow (Add t e) = prettyShow t <> " + " <> prettyShow e
  prettyShow (Sub t e) = prettyShow t <> " - " <> prettyShow e
  prettyShow (NoExpr t) = prettyShow t
  prettyShow (ExprMisc e) = prettyShow e

instance PrettyShow Term where
  prettyShow (Multi f t) = prettyShow f <> " * " <> prettyShow t
  prettyShow (Div f t) = prettyShow f <> " / " <> prettyShow t
  prettyShow (NoTerm f) = prettyShow f

instance PrettyShow Func where
  prettyShow (Func s n) = funcInfoName s <> "(" <> intercalate ", " (prettyShow <$> n) <> ")"
  prettyShow (NoFunc b) = prettyShow b

instance PrettyShow Negation where
  prettyShow (Neg expo) = "-" <> prettyShow expo
  prettyShow (NoNeg expo) = prettyShow expo

instance PrettyShow Expo where
  prettyShow (NoExpo b) = prettyShow b
  prettyShow (Expo b expo) = prettyShow b <> " ^ " <> prettyShow expo

instance PrettyShow NumBase where
  prettyShow (NBParen p) = prettyShow p
  prettyShow (Value i) = fromString $ show i

instance (PrettyShow a) => PrettyShow (Paren a) where
  prettyShow (Paren a) = "(" <> prettyShow a <> ")"

instance PrettyShow Base where
  prettyShow (NBase nb) = prettyShow nb
  prettyShow (DiceBase dop) = prettyShow dop
  prettyShow (NumVar t) = t

instance PrettyShow Die where
  prettyShow (Die b) = "d" <> prettyShow b
  prettyShow (CustomDie lv) = "d" <> prettyShow lv
  -- prettyShow (CustomDie is) = "d{" <> intercalate ", " (prettyShow <$> is) <> "}"
  prettyShow (LazyDie d) = "d!" <> T.tail (prettyShow d)

instance PrettyShow Dice where
  prettyShow (Dice b d dor) = prettyShow b <> prettyShow d <> helper' dor
    where
      fromOrdering ao = M.findWithDefault "??" ao $ snd advancedOrderingMapping
      fromLHW (Where o i) = "w" <> fromOrdering o <> prettyShow i
      fromLHW (Low i) = "l" <> prettyShow i
      fromLHW (High i) = "h" <> prettyShow i
      helper' Nothing = ""
      helper' (Just (DieOpRecur dopo' dor')) = helper dopo' <> helper' dor'
      helper (DieOpOptionLazy doo) = "!" <> helper doo
      helper (Reroll True o i) = "ro" <> fromOrdering o <> prettyShow i
      helper (Reroll False o i) = "rr" <> fromOrdering o <> prettyShow i
      helper (DieOpOptionKD Keep lhw) = "k" <> fromLHW lhw
      helper (DieOpOptionKD Drop lhw) = "d" <> fromLHW lhw

instance (PrettyShow a, PrettyShow b) => PrettyShow (Either a b) where
  prettyShow (Left a) = prettyShow a
  prettyShow (Right b) = prettyShow b

instance (PrettyShow a) => PrettyShow (Var a) where
  prettyShow (Var t a) = "var " <> t <> " = " <> prettyShow a
  prettyShow (VarLazy t a) = "var !" <> t <> " = " <> prettyShow a

instance (PrettyShow b) => PrettyShow (If b) where
  prettyShow (If b t e) = "if " <> prettyShow b <> " then " <> prettyShow t <> " else " <> prettyShow e

instance PrettyShow Statement where
  prettyShow (StatementExpr l) = prettyShow l <> "; "
  prettyShow (StatementListValues l) = prettyShow l <> "; "

instance PrettyShow Program where
  prettyShow (Program ss a) = foldr ((<>) . prettyShow) (prettyShow a) ss
