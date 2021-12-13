module Tablebot.Plugin.Dice.DiceEval where

import Control.Monad (when)
import Control.Monad.Exception (MonadException)
import Data.Bifunctor (Bifunctor (second))
import Data.List (genericDrop, genericReplicate, genericTake, intercalate, sortBy)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, (<|))
import Data.Map as M (findWithDefault)
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import System.Random (randomRIO)
import Tablebot.Plugin.Dice.DiceData
import Tablebot.Plugin.Dice.DiceFunctions (ListInteger (..), getFunc)
import Tablebot.Plugin.Discord (Format (..), formatInput, formatText)
import Tablebot.Plugin.Exception (BotException (EvaluationException), catchBot, throwBot)
import Tablebot.Plugin.Random (chooseOne)

-- | The maximum depth that should be permitted. Used to limit number of dice and rerolls.
maximumRNG :: Integer
maximumRNG = 150

-- | Check whether the RNG count has been exceeded by the integer given.
checkRNGCount :: Integer -> IO ()
checkRNGCount i =
  when (i > maximumRNG) $ throwBot $ EvaluationException ("exceeded maximum rng count (" ++ show maximumRNG ++ ")") []

--- Evaluating an expression. Uses IO because dice are random

-- | Given an expression, evaluate it, getting the pretty printed string and the value of
-- the result
evalListValues :: ListValues -> IO ([Integer], [Text])
evalListValues lv = do
  (is, ss, _) <- evalShowListValues 0 lv
  let ret = toOut is ss
  return $
    if countAllFormatting ret < 199
      then unzip (second pack <$> ret)
      else (fst <$> ret, [pack $ prettyShow lv ++ " `[could not display rolls]`"])
  where
    toOut (LIInteger i) ss' = zip [i] ss'
    toOut (LIList is') ss' = zip is' ss'
    countFormatting :: String -> Int
    countFormatting s = (`div` 4) $ foldr (\c cf -> cf + fromEnum (c `elem` ['~', '_', '*'])) 0 s
    countAllFormatting lst = foldr (\(_, s) tot -> countFormatting s + tot) 0 lst

-- | Utility function to display dice.
--
-- The tuple of integers denotes what the critvalues of this dice value are. The `a`
-- denotes the value that is being printed, and needs to have `PrettyShow` defined for it.
-- Finally, the list of tuples denotes all the values that the `a` value has gone through.
-- If the `Maybe Bool` value is `Nothing`, the number is displayed as normal. If the value
-- is `Just False`, the value has been rerolled over, and is displayed crossed out. If the
-- value is `Just True`, the value has been dropped, and the number is crossed out and
-- underlined.
dieShow :: (PrettyShow a, MonadException m) => Maybe (Integer, Integer) -> a -> [(Integer, Maybe Bool)] -> m String
dieShow _ a [] = throwBot $ EvaluationException "tried to show empty set of results" [prettyShow a]
dieShow lchc d ls = return $ prettyShow d ++ " [" ++ intercalate ", " adjustList ++ "]"
  where
    toCrit =
      if isNothing lchc
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

evalShowList :: (IOEval a, PrettyShow a) => Integer -> [a] -> IO ([Integer], String, Integer)
evalShowList rngCount as = evalShowList' rngCount as >>= \(is, ss, rc) -> return (is, intercalate ", " ss, rc)

evalShowList' :: (IOEval a, PrettyShow a) => Integer -> [a] -> IO ([Integer], [String], Integer)
evalShowList' = evalShowList'' evalShow

evalShowList'' :: (Integer -> a -> IO (i, s, Integer)) -> Integer -> [a] -> IO ([i], [s], Integer)
evalShowList'' customEvalShow rngCount = foldr foldF (return ([], [], rngCount))
  where
    foldF a sumrngcount = do
      (diceSoFar, ss, rngCountTotal) <- sumrngcount
      (i, s, rngCountTemp) <- customEvalShow rngCountTotal a
      return (i : diceSoFar, s : ss, rngCountTemp)

evalShowListValues :: Integer -> ListValues -> IO (ListInteger, [String], Integer)
evalShowListValues rngCount (NoList expr) = evalShow rngCount expr >>= \(i, s, rc) -> return (LIInteger i, [s], rc)
evalShowListValues rngCount (MultipleValues nb b) = do
  (nb', _, rngCount') <- evalShow rngCount nb
  (is, ss, rc) <- evalShowList' rngCount' (genericReplicate nb' b)
  return (LIList is, ss, rc)
evalShowListValues rngCount (LVList es) = do
  (is, ss, rc) <- evalShowList' rngCount es
  return (LIList is, ss, rc)

-- | This type class gives a function which evaluates the value to an integer and a
-- string.
class IOEval a where
  -- | Evaluate the given item to an integer, a string representation of the value, and
  -- the number of RNG calls it took. If the `a` value is a dice value, the values of the
  -- dice should be displayed. The integer given initially is the current RNG count of the
  -- expression. This function adds the current location to the exception callstack.
  evalShow :: PrettyShow a => Integer -> a -> IO (Integer, String, Integer)
  evalShow rngCount a = catchBot (evalShow' rngCount a) handleException
    where
      handleException (EvaluationException msg' locs) = throwBot (EvaluationException msg' (addIfNotIn locs))
      handleException e = throwBot e
      pa = prettyShow a
      addIfNotIn locs = if null locs || pa /= Prelude.head locs then pa : locs else locs

  evalShow' :: Integer -> a -> IO (Integer, String, Integer)

instance IOEval Base where
  evalShow' rngCount (NBase nb) = evalShow rngCount nb
  evalShow' rngCount (DiceBase dice) = evalShow rngCount dice

instance IOEval Die where
  evalShow' rngCount ld@(LazyDie d) = do
    (i, _, rngCount') <- evalShow rngCount d
    ds <- dieShow Nothing ld [(i, Nothing)]
    return (i, ds, rngCount')
  evalShow' rngCount d@(CustomDie is) = do
    i <- chooseOne is
    (i', _, rngCount') <- evalShow rngCount i
    ds <- dieShow Nothing d [(i', Nothing)]
    checkRNGCount (rngCount' + 1)
    return (i', ds, rngCount' + 1)
  evalShow' rngCount d@(Die b) = do
    (bound, _, rngCount') <- evalShow rngCount b
    if bound < 1
      then throwBot $ EvaluationException ("Cannot roll a < 1 sided die (" ++ formatText Code (prettyShow b) ++ ")") []
      else do
        i <- randomRIO (1, bound)
        ds <- dieShow Nothing d [(i, Nothing)]
        checkRNGCount (rngCount' + 1)
        return (i, ds, rngCount' + 1)

instance IOEval Dice where
  evalShow' rngCount dop = do
    (lst, mnmx, rngCount') <- evalDieOp rngCount dop
    let vs = fromEvalDieOpList lst
    s <- dieShow mnmx dop vs
    return (sum (fst <$> filter (isNothing . snd) vs), s, rngCount')

-- | Utility function to transform the output list type of other utility functions into
-- one that `dieShow` recognises
fromEvalDieOpList :: [(NonEmpty Integer, Bool)] -> [(Integer, Maybe Bool)]
fromEvalDieOpList = foldr foldF []
  where
    foldF (is, b) lst = let is' = (,Just False) <$> NE.tail is in (reverse ((NE.head is, if b then Nothing else Just True) : is') ++ lst)

-- | Helper function that takes a set of Dice and returns a tuple of three items. The
-- second item is the maximum and minimum value of the base die.
-- The first item is a list representing each die - a tuple with a history of the die
-- being rolled, and whether the die has been dropped or not. The first item of each die
-- record is the current value of the die. The last item of the tuple is how many calls to
-- RNG there have been.
--
-- The function itself checks to make sure the number of dice being rolled is less than
-- the maximum recursion and is non-negative.
evalDieOp :: Integer -> Dice -> IO ([(NonEmpty Integer, Bool)], Maybe (Integer, Integer), Integer)
evalDieOp rngCount (Dice b ds dopo) = do
  (nbDice, _, rngCountB) <- evalShow rngCount b
  if nbDice > maximumRNG
    then throwBot (EvaluationException ("tried to roll more than " ++ formatInput Code maximumRNG ++ " dice: " ++ formatInput Code nbDice) [prettyShow b])
    else do
      if nbDice < 0
        then throwBot (EvaluationException ("tried to give a negative value to the number of dice: " ++ formatInput Code nbDice) [prettyShow b])
        else do
          (ds', rngCountCondense, crits) <- condenseDie rngCountB ds
          (rolls, _, rngCountRolls) <- evalShowList rngCountCondense (genericReplicate nbDice ds')
          let vs = fmap (\i -> (i :| [], True)) rolls
          (rs, rngCountRs) <- evalDieOp' rngCountRolls dopo ds' vs
          return (sortBy sortByOption rs, crits, rngCountRs)
  where
    condenseDie rngCount' (Die dBase) = do
      (i, _, rngCount'') <- evalShow rngCount' dBase
      return (Die (Value i), rngCount'', Just (1, i))
    condenseDie rngCount' (CustomDie is) = do
      -- (is', rngCount'') <- foldr foldF (return ([], rngCount')) is
      (is', _, rngCount'') <- evalShowList rngCount' is
      return (CustomDie (fromIntegerToExpr <$> is'), rngCount'', Nothing)
    condenseDie rngCount' (LazyDie d) = return (d, rngCount', Nothing)
    -- foldF die sumrngcount = do
    --   (diceSoFar, rngCountTotal) <- sumrngcount
    --   (i, _, rngCountTemp) <- evalShow rngCountTotal die
    --   return (i : diceSoFar, rngCountTemp)
    sortByOption (e :| es, _) (f :| fs, _)
      | e == f = compare (length fs) (length es)
      | otherwise = compare e f

-- | Utility function that processes a `Maybe DieOpRecur`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp' :: Integer -> Maybe DieOpRecur -> Die -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp' rngCount Nothing _ is = return (is, rngCount)
evalDieOp' rngCount (Just (DieOpRecur doo mdor)) die is = do
  (doo', rngCount') <- processDOO rngCount doo
  (is', rngCount'') <- evalDieOp'' rngCount' doo' die is
  evalDieOp' rngCount'' mdor die is'
  where
    processLHW rngCount' (Low i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Low (Value i'), rngCount'')
    processLHW rngCount' (High i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (High (Value i'), rngCount'')
    processLHW rngCount' (Where o i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Where o (Value i'), rngCount'')
    processDOO rngCount' (DieOpOptionKD kd lhw) = do
      (lhw', rngCount'') <- processLHW rngCount' lhw
      return (DieOpOptionKD kd lhw', rngCount'')
    processDOO rngCount' (Reroll once o i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Reroll once o (Value i'), rngCount'')
    processDOO rngCount' (DieOpOptionLazy doo') = return (doo', rngCount')

-- | Utility function that processes a `DieOpOption`, when given a die, and dice that have
-- already been processed.
evalDieOp'' :: Integer -> DieOpOption -> Die -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp'' rngCount (DieOpOptionLazy doo) die is = evalDieOp'' rngCount doo die is
evalDieOp'' rngCount (DieOpOptionKD kd lhw) _ is = evalDieOpHelpKD rngCount kd lhw is
evalDieOp'' rngCount (Reroll once o i) die is = foldr rerollF (return ([], rngCount)) is
  where
    rerollF g@(i', b) isRngCount' = do
      (is', rngCount') <- isRngCount'
      (iEval, _, rngCount'') <- evalShow rngCount' i
      if b && applyCompare o (NE.head i') iEval
        then do
          (v, _, rngCount''') <- evalShow rngCount'' die
          let ret = (v <| i', b)
          if once
            then return (ret : is', rngCount''')
            else rerollF ret (return (is', rngCount'''))
        else return (g : is', rngCount'')

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
evalDieOpHelpKD :: Integer -> KeepDrop -> LowHighWhere -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOpHelpKD rngCount kd (Where cmp i) is = foldr foldF (return ([], rngCount)) is
  where
    isKeep = if kd == Keep then id else not
    foldF (iis, b) sumrngcount = do
      (diceSoFar, rngCountTotal) <- sumrngcount
      (i', _, rngCountTemp) <- evalShow rngCountTotal i
      return ((iis, b && isKeep (applyCompare cmp (NE.head iis) i')) : diceSoFar, rngCountTemp)
evalDieOpHelpKD rngCount kd lh is = do
  (i', _, rngCount') <- evalShow rngCount i
  return (d ++ setToDropped (getDrop i' sk) ++ getKeep i' sk, rngCount')
  where
    (k, d) = separateKeptDropped is
    order l l' = if isLow lh then compare l l' else compare l' l
    sk = sortBy order k
    i = fromMaybe (Value 0) (getValueLowHigh lh)
    (getDrop, getKeep) = if kd == Keep then (genericDrop, genericTake) else (genericTake, genericDrop)

--- Pure evaluation functions for non-dice calculations
-- Was previously its own type class that wouldn't work for evaluating Base values.

binOpHelp :: (IOEval a, IOEval b, PrettyShow a, PrettyShow b) => Integer -> a -> b -> String -> (Integer -> Integer -> Integer) -> IO (Integer, String, Integer)
binOpHelp rngCount a b opS op = do
  (a', a's, rngCount') <- evalShow rngCount a
  (b', b's, rngCount'') <- evalShow rngCount' b
  return (op a' b', a's ++ " " ++ opS ++ " " ++ b's, rngCount'')

instance IOEval Expr where
  evalShow' rngCount (NoExpr t) = evalShow rngCount t
  evalShow' rngCount (Add t e) = binOpHelp rngCount t e "+" (+)
  evalShow' rngCount (Sub t e) = binOpHelp rngCount t e "-" (-)

instance IOEval Term where
  evalShow' rngCount (NoTerm f) = evalShow rngCount f
  evalShow' rngCount (Multi f t) = binOpHelp rngCount f t "*" (*)
  evalShow' rngCount (Div f t) = do
    (f', f's, rngCount') <- evalShow rngCount f
    (t', t's, rngCount'') <- evalShow rngCount' t
    if t' == 0
      then throwBot (EvaluationException "division by zero" [prettyShow t])
      else return (div f' t', f's ++ " / " ++ t's, rngCount'')

instance IOEval Func where
  -- evalShow' rngCount (Func "id" neg) = evalShow rngCount neg
  -- evalShow' rngCount (Func "fact" expr) = do
  --   -- (neg', neg's, rngCount') <- evalShow rngCount neg
  --   (exprs, s, rngCount') <- evalShowList rngCount expr
  --   if neg' > factorialLimit
  --     then throwBot $ EvaluationException ("tried to evaluate a factorial with input number greater than the limit (" ++ formatInput Code factorialLimit ++ "): `" ++ formatInput Code neg' ++ "`") [prettyShow neg]
  --     else do
  --       f <- getFunc "fact" exprs
  --       return (f, "fact" ++ " " ++ neg's, rngCount')
  evalShow' rngCount (Func s exprs) = do
    (exprs', _, rngCount') <- evalShowList'' evalShowListValues rngCount exprs
    f <- getFunc (pack s) exprs'
    return (f, s ++ "(" ++ intercalate ", " (prettyShow <$> exprs) ++ ")", rngCount')
  evalShow' rngCount (NoFunc b) = evalShow rngCount b

instance IOEval Negation where
  evalShow' rngCount (NoNeg expo) = evalShow rngCount expo
  evalShow' rngCount (Neg expo) = do
    (expo', expo's, rngCount') <- evalShow rngCount expo
    return (negate expo', "-" ++ expo's, rngCount')

instance IOEval Expo where
  evalShow' rngCount (NoExpo b) = evalShow rngCount b
  evalShow' rngCount (Expo b expo) = do
    (expo', expo's, rngCount') <- evalShow rngCount expo
    if expo' < 0
      then throwBot (EvaluationException ("the exponent is negative: " ++ formatInput Code expo') [prettyShow expo])
      else do
        (b', b's, rngCount'') <- evalShow rngCount' b
        return (b' ^ expo', b's ++ " ^ " ++ expo's, rngCount'')

instance IOEval NumBase where
  evalShow' rngCount (Paren e) = do
    (r, s, rngCount') <- evalShow rngCount e
    return (r, "(" ++ s ++ ")", rngCount')
  evalShow' rngCount (Value i) = return (i, show i, rngCount)

--- Pretty printing the AST
-- The output from this should be parseable

-- | Type class to display an expression prettily (not neccessarily accurately).
class PrettyShow a where
  -- | Print the given value prettily.
  prettyShow :: a -> String

instance PrettyShow ListValues where
  prettyShow (NoList e) = prettyShow e
  prettyShow (MultipleValues nb b) = prettyShow nb <> "#" <> prettyShow b
  prettyShow (LVList es) = "{" <> intercalate "," (prettyShow <$> es) <> "}"

instance PrettyShow Expr where
  prettyShow (Add t e) = prettyShow t <> " + " <> prettyShow e
  prettyShow (Sub t e) = prettyShow t <> " - " <> prettyShow e
  prettyShow (NoExpr t) = prettyShow t

instance PrettyShow Term where
  prettyShow (Multi f t) = prettyShow f <> " * " <> prettyShow t
  prettyShow (Div f t) = prettyShow f <> " / " <> prettyShow t
  prettyShow (NoTerm f) = prettyShow f

instance PrettyShow Func where
  -- prettyShow (Func "id" n) = prettyShow n
  prettyShow (Func s n) = s <> "(" <> intercalate "," (prettyShow <$> n) <> ")"
  prettyShow (NoFunc b) = prettyShow b

instance PrettyShow Negation where
  prettyShow (Neg expo) = "-" <> prettyShow expo
  prettyShow (NoNeg expo) = prettyShow expo

instance PrettyShow Expo where
  prettyShow (NoExpo b) = prettyShow b
  prettyShow (Expo b expo) = prettyShow b <> " ^ " <> prettyShow expo

instance PrettyShow NumBase where
  prettyShow (Paren e) = "(" <> prettyShow e <> ")"
  prettyShow (Value i) = fromString $ show i

instance PrettyShow Base where
  prettyShow (NBase nb) = prettyShow nb
  prettyShow (DiceBase dop) = prettyShow dop

instance PrettyShow Die where
  prettyShow (Die b) = "d" <> prettyShow b
  prettyShow (CustomDie is) = "d{" <> intercalate ", " (prettyShow <$> is) <> "}"
  prettyShow (LazyDie d) = "d!" ++ Prelude.tail (prettyShow d)

instance PrettyShow Dice where
  prettyShow (Dice b d dor) = prettyShow b <> prettyShow d <> helper' dor
    where
      fromOrdering ao = M.findWithDefault "??" ao $ snd advancedOrderingMapping
      fromLHW (Where o i) = "w" <> fromOrdering o <> prettyShow i
      fromLHW (Low i) = "l" <> fromString (prettyShow i)
      fromLHW (High i) = "h" <> fromString (prettyShow i)
      helper' Nothing = ""
      helper' (Just (DieOpRecur dopo' dor')) = helper dopo' <> helper' dor'
      helper (DieOpOptionLazy doo) = "!" <> helper doo
      helper (Reroll True o i) = "ro" <> fromOrdering o <> prettyShow i
      helper (Reroll False o i) = "rr" <> fromOrdering o <> prettyShow i
      helper (DieOpOptionKD Keep lhw) = "k" <> fromLHW lhw
      helper (DieOpOptionKD Drop lhw) = "d" <> fromLHW lhw