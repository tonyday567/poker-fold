{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Exact best-response solver for a simplified preflop action model:
-- Fold / Call / Raise 3bb / All-in, with 10bb starting stacks.
--
-- The strategy space is threshold-based: for each of 6 decision contexts a
-- sorted list of equity cutoffs determines which action each hand takes.
-- Because there are only 169 distinct starting-hand classes, the best response
-- in any one context can be found exactly by dynamic programming over the
-- ordered hand classes.  We then cycle through contexts (coordinate ascent)
-- and alternate optimising SB and BB to approach a Nash equilibrium.
module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, when)
import Control.Monad.State.Lazy (State, evalState, state)
import Data.Function (on)
import Data.Functor.Rep (index)
import Data.List (foldl', maximumBy, minimumBy, sort, sortOn)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Optics.Core (review, view)
import Poker.Card (Hole)
import Poker.EquityMatrix (EqMatrix, contextActionValues, evAnalyticalWith, readEquityMatrix, disjointHoles)
import Poker.Range (Range, StartingHandS (..), fromHole, readSomeRanges, startingHandI, toHoles)
import Poker.Strategy (Thresh4, evStrategyWith, playOneWith)
import Poker.Table (RawAction (..), Table (..))
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import System.Random (RandomGen, mkStdGen)
import Prelude

-- | Simplified preflop actions.
data Action3 = Fold3 | Call3 | Raise3 | AllIn3
  deriving (Eq, Show, Enum, Ord)

-- | Raise 3bb means a total bet of 3.0 (the big blind is 1.0, so the raise
-- amount above the current max bet is 2.0).  All-in for a 10bb stack is a
-- total bet of 10.0, i.e. a raise of 9.0 above the big blind.
toRaw3 :: Action3 -> RawAction
toRaw3 Fold3 = RawFold
toRaw3 Call3 = RawCall
toRaw3 Raise3 = RawRaise 2.0
toRaw3 AllIn3 = RawRaise 9.0

-- | Decision contexts for the 3bb model.  Even indices are seat 0 (SB),
-- odd indices are seat 1 (BB).
--
--   0: SB first action (no prior raise)
--   1: BB first action after SB call/check (no prior raise)
--   2: BB facing an SB raise to 3bb
--   3: BB facing an SB all-in
--   4: SB facing a BB raise to 3bb
--   5: SB facing a BB all-in
--
-- All-ins are detected by the opponent having zero remaining stack.
decisionContext3bb :: Table -> Maybe Int
decisionContext3bb t = case cursor t of
  Nothing -> Nothing
  Just p ->
    let opponent = 1 - p
        isRaise (RawRaise _, _) = True
        isRaise _ = False
        raised = any isRaise (history t)
        oppAllIn = stacks t List.!! opponent <= 0
     in Just $ case p of
          0 -> if not raised then 0 else if oppAllIn then 5 else 4
          1 -> if not raised then 1 else if oppAllIn then 3 else 2
          _ -> error "decisionContext3bb: only two seats supported"

-- | Intended action from a threshold tuple and a hand-strength value.
-- The four thresholds are interpreted as:
--   * e < t0 : fold
--   * t0 <= e < t1 : call
--   * t1 <= e < t2 : raise 3bb
--   * otherwise : all-in
-- strategyAction3 maps these to context-legal raw actions.
intendedAction3 :: Thresh4 -> Double -> Action3
intendedAction3 (t0, t1, t2, _) e
  | e < t0 = Fold3
  | e < t1 = Call3
  | e < t2 = Raise3
  | otherwise = AllIn3

-- | A full strategy is one threshold tuple for each of the 6 preflop decision
-- contexts.
newtype Strategy3 = Strategy3
  { thresholds3 :: [Thresh4]
  }
  deriving (Eq, Show)

-- | Map an intended action to the actual raw action, respecting the legal
-- actions available in each context.  'qs' is the per-hand-class quantile
-- (cumulative hand-combo percentage weaker than this hand).
--
-- For C4/C5 (SB responding after calling in C0) the thresholds are stored as
-- offsets above the C0 call threshold: the effective threshold is
--   C0_call_start + stored_threshold
-- This keeps response thresholds tied to the hands that actually reach them.
strategyAction3 :: [Double] -> Strategy3 -> Int -> Hole -> RawAction
strategyAction3 qs strat ctx hole =
  let StartingHandS handIdx = view startingHandI (fromHole hole)
      q = qs !! fromIntegral handIdx
      (c0t0, _, _, _) = thresholds3 strat !! 0
      rawThresholds
        | ctx == 4 || ctx == 5 =
            let (t0, t1, t2, t3) = thresholds3 strat !! ctx
                offset t = max 0.0 (min 1.0 (c0t0 + t))
             in (offset t0, offset t1, offset t2, offset t3)
        | otherwise = thresholds3 strat !! ctx
      (t0, t1, t2, _) = rawThresholds
      intended
        | q < t0 = Fold3
        | q < t1 = Call3
        | q < t2 = Raise3
        | otherwise = AllIn3
   in case ctx of
        0 -> toRaw3 intended
        1 -> case intended of
          Fold3 -> RawCall
          Call3 -> RawCall
          Raise3 -> RawRaise 2.0
          AllIn3 -> RawRaise 9.0
        2 -> case intended of
          Fold3 -> RawFold
          Call3 -> RawCall
          _ -> RawRaise 9.0
        3 -> case intended of
          Fold3 -> RawFold
          _ -> RawCall
        4 -> case intended of
          Fold3 -> RawFold
          Call3 -> RawCall
          _ -> RawRaise 9.0
        5 -> case intended of
          Fold3 -> RawFold
          _ -> RawCall
        _ -> error "strategyAction3: unknown context"

-- | Effective actions for each context, in the order they appear on the
-- hand-strength axis.  Some intended actions collapse to the same raw action
-- in restricted contexts.
contextActions :: Int -> [Action3]
contextActions ctx = case ctx of
  0 -> [Fold3, Call3, Raise3, AllIn3]
  1 -> [Call3, Raise3, AllIn3]
  2 -> [Fold3, Call3, AllIn3]
  3 -> [Fold3, Call3]
  4 -> [Fold3, Call3, AllIn3]
  5 -> [Fold3, Call3]
  _ -> error "contextActions: unknown context"


-- | Raw action corresponding to an intended action in a given context.
contextRawAction :: Int -> Action3 -> RawAction
contextRawAction ctx a = case ctx of
  0 -> toRaw3 a
  1 -> case a of
    Fold3 -> RawCall
    Call3 -> RawCall
    Raise3 -> RawRaise 2.0
    AllIn3 -> RawRaise 9.0
  2 -> case a of
    Fold3 -> RawFold
    Call3 -> RawCall
    _ -> RawRaise 9.0
  3 -> case a of
    Fold3 -> RawFold
    _ -> RawCall
  4 -> case a of
    Fold3 -> RawFold
    Call3 -> RawCall
    _ -> RawRaise 9.0
  5 -> case a of
    Fold3 -> RawFold
    _ -> RawCall
  _ -> error "contextRawAction: unknown context"

playOne3 :: S.Vector Word16 -> [Double] -> Strategy3 -> Strategy3 -> Table -> Double
playOne3 hvs qs hero opp =
  playOneWith hvs decisionContext3bb (\p ctx hole -> strategyAction3 qs (if p == 0 then hero else opp) ctx hole)

evStrategy3 :: (RandomGen g) => S.Vector Word16 -> [Double] -> Strategy3 -> Strategy3 -> Int -> State g Double
evStrategy3 hvs qs hero opp n =
  evStrategyWith hvs decisionContext3bb (\p ctx hole -> strategyAction3 qs (if p == 0 then hero else opp) ctx hole) n

runEv3 :: S.Vector Word16 -> [Double] -> Strategy3 -> Strategy3 -> Int -> Double
runEv3 hvs qs hero opp n = evalState (evStrategy3 hvs qs hero opp n) (mkStdGen 123)

-- | Deterministic analytical EV for the 3bb model, using the class-average
-- equity matrix.  No MC noise.
scoreAnalytical3 :: EqMatrix -> [Double] -> Strategy3 -> Strategy3 -> Double
scoreAnalytical3 eq qs hero opp =
  evAnalyticalWith eq decisionContext3bb (\p ctx hole -> strategyAction3 qs (if p == 0 then hero else opp) ctx hole)

pct :: Double -> String
pct x = show (round (x * 100) :: Int)

pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

-- | Quantile (cumulative combo-weight of all strictly weaker hands) for every
-- hand class, indexed by hand-class number.
handQuantiles :: Range Double -> Range Double -> [Double]
handQuantiles o2 weights =
  let n = 169
      eq i = index o2 (StartingHandS (fromIntegral i))
      wt i = index weights (StartingHandS (fromIntegral i))
      order = sortOn eq [0 .. n - 1]
      sortedWts = map wt order
      total = sum sortedWts
      cum = scanl (+) 0 sortedWts
      quantMap = Map.fromList [(order !! k, cum !! k / total) | k <- [0 .. n - 1]]
   in [quantMap Map.! i | i <- [0 .. n - 1]]

-- | Compact display of action frequencies for each context.
compactStrategy3 :: Strategy3 -> String
compactStrategy3 (Strategy3 ts) = unwords $ zipWith fmt [0 .. 5 :: Int] ts
  where
    fmt 0 (t0, t1, t2, t3) =
      "C0: " ++ unwords (pct <$> [t0, t1 - t0, t2 - t1, t3 - t2, 1 - t3])
    fmt 1 (_, t1, t2, t3) =
      "C1: " ++ unwords (pct <$> [t1, t2 - t1, t3 - t2, 1 - t3])
    fmt 2 (t0, t1, _, _) =
      "C2: " ++ unwords (pct <$> [t0, t1 - t0, 1 - t1])
    fmt 3 (t0, _, _, _) =
      "C3: " ++ unwords (pct <$> [t0, 1 - t0])
    fmt 4 (t0, t1, _, _) =
      "C4: " ++ unwords (pct <$> [t0, t1 - t0, 1 - t1])
    fmt 5 (t0, _, _, _) =
      "C5: " ++ unwords (pct <$> [t0, 1 - t0])
    fmt _ _ = ""

-- | Order of hand classes by equity (weakest first).
equityOrder :: Range Double -> [Int]
equityOrder o2 = sortOn (\i -> index o2 (StartingHandS (fromIntegral i))) [0 .. 168]

-- | Marginal EV contribution of every hand class taking every effective action
-- in one context, holding everything else fixed.
--
-- Result: one list per effective action; each inner list is indexed by hand
-- class (0..168) and contains the weighted sum of payoffs for that hand class
-- choosing that action in the given context.
contextMarginals3 :: EqMatrix -> [Double] -> Strategy3 -> Strategy3 -> Int -> [[Double]]
contextMarginals3 eq qs hero opp ctx =
  let actions = contextActions ctx
      raws = map (contextRawAction ctx) actions
      choose p c h = strategyAction3 qs (if p == 0 then hero else opp) c h
      holes = fmap (toHoles . review startingHandI . StartingHandS . fromIntegral) [0 .. 168 :: Int]
      totalPairs = 2652.0 * 2450.0
      weight hs vs = fromIntegral (length [(h, v) | h <- hs, v <- vs, disjointHoles h v]) / totalPairs
      nActs = length actions
      emptyAcc = replicate nActs (replicate 169 0.0)
      updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
      accum acc hi vi =
        let wh = weight (holes !! hi) (holes !! vi)
            vals = contextActionValues eq decisionContext3bb choose raws ctx hi vi
         in case vals of
              [_] -> acc
              _ ->
                let hidx = if ctx `elem` [0, 4, 5] then hi else vi
                    add v as = updateAt hidx (as !! hidx + wh * v) as
                 in zipWith add vals acc
   in foldl' (\acc hi -> foldl' (\acc' vi -> accum acc' hi vi) acc [0 .. 168]) emptyAcc [0 .. 168]

-- | Convert a list of boundary values (one per gap between effective actions)
-- into the full 4-threshold tuple for a specific context.
cutsToThresh4 :: Int -> [Double] -> Thresh4
cutsToThresh4 ctx ts = case ctx of
  0 -> case ts of [c0, c1, c2] -> (c0, c1, c2, 1.0); _ -> error "cutsToThresh4: C0 needs 3 cuts"
  1 -> case ts of [c1, c2] -> (c1, c1, c2, 1.0); _ -> error "cutsToThresh4: C1 needs 2 cuts"
  2 -> case ts of [c0, c1] -> (c0, c1, 1.0, 1.0); _ -> error "cutsToThresh4: C2 needs 2 cuts"
  3 -> case ts of [c0] -> (c0, 1.0, 1.0, 1.0); _ -> error "cutsToThresh4: C3 needs 1 cut"
  4 -> case ts of [c0, c1] -> (c0, c1, 1.0, 1.0); _ -> error "cutsToThresh4: C4 needs 2 cuts"
  5 -> case ts of [c0] -> (c0, 1.0, 1.0, 1.0); _ -> error "cutsToThresh4: C5 needs 1 cut"
  _ -> error "cutsToThresh4: unknown context"

-- | Find the exact optimal quantile-threshold tuple for one context by DP over
-- the ordered hand classes.  'maximize' is True for SB and False for BB.
optimalContextThresholds :: Range Double -> Range Double -> Int -> [[Double]] -> Bool -> Thresh4
optimalContextThresholds o2 weights ctx vals maximize =
  let order = equityOrder o2
      wts = map (\i -> index weights (StartingHandS (fromIntegral i))) order
      total = sum wts
      cum = scanl (+) 0 wts
      l = length vals
      n = 169
      inf = 1e100
      extreme = if maximize then maximumBy else minimumBy
      initDP = 0 : replicate (l - 1) (if maximize then -inf else inf)
      step (dps, prevs) pos =
        let i = order !! pos
            v = map (!! i) vals
            prevDp = last dps
            (newDp, newPrev) =
              unzip
                [ let candidates = [(prevDp !! a', a') | a' <- [0 .. a]]
                      (bestVal, bestA) = extreme (compare `on` fst) candidates
                   in (v !! a + bestVal, bestA)
                  | a <- [0 .. l - 1]
                ]
         in (dps ++ [newDp], prevs ++ [newPrev])
      (dps, prevs) = foldl' step ([initDP], []) [0 .. n - 1]
      finalDp = last dps
      bestA = snd $ extreme (compare `on` fst) (zip finalDp [0 ..])
      assign = backtrack n bestA []
      backtrack 0 _ acc = acc
      backtrack pos a acc =
        let prevA = prevs !! (pos - 1) !! a
         in backtrack (pos - 1) prevA (a : acc)
      -- quantile boundary: cumulative combo weight of the lower band
      boundary k =
        case List.findIndex (>= k) assign of
          Nothing -> 1.0
          Just 0 -> 0.0
          Just pos -> (cum !! pos) / total
      cuts = [boundary k | k <- [1 .. l - 1]]
   in cutsToThresh4 ctx cuts

-- | Optimise a single context, returning a new strategy with only that context
-- changed.
bestResponseContext3 :: EqMatrix -> Range Double -> Range Double -> [Double] -> Strategy3 -> Strategy3 -> Int -> Bool -> Strategy3
bestResponseContext3 eq o2 weights qs hero opp ctx maximize =
  let vals = contextMarginals3 eq qs hero opp ctx
      c0t0 = threshComponent (thresholds3 hero !! 0) 0
      rawTup = optimalContextThresholds o2 weights ctx vals maximize
      -- C4/C5 thresholds are stored as offsets above C0's call threshold so
      -- that response ranges stay tied to the hands that actually reach them.
      newTup
        | ctx == 4 || ctx == 5 =
            let (a, b, c, d) = rawTup
                off x = max 0.0 (min 1.0 (x - c0t0))
             in (off a, off b, off c, off d)
        | otherwise = rawTup
      s = if ctx `elem` [0, 4, 5] then hero else opp
      ts = thresholds3 s
   in Strategy3 (take ctx ts ++ [newTup] ++ drop (ctx + 1) ts)

-- | Exact coordinate-ascent best response for seat 0 (SB): cycle through
-- contexts 0, 4, 5 until no context can improve EV.
bestResponseSeat03 :: EqMatrix -> Range Double -> Range Double -> [Double] -> Strategy3 -> Strategy3 -> Strategy3
bestResponseSeat03 eq o2 weights qs hero opp = go hero (scoreAnalytical3 eq qs hero opp) (0 :: Int)
  where
    go s lastEV iter
      | iter > 20 = s
      | otherwise =
          let s' = foldl' (\s0 ctx -> bestResponseContext3 eq o2 weights qs s0 opp ctx True) s [0, 4, 5]
              ev = scoreAnalytical3 eq qs s' opp
           in if abs (ev - lastEV) < 1e-7 then s' else go s' ev (iter + 1)

-- | Exact coordinate-ascent best response for seat 1 (BB): cycle through
-- contexts 1, 2, 3 until no context can reduce seat-0 EV.
bestResponseSeat13 :: EqMatrix -> Range Double -> Range Double -> [Double] -> Strategy3 -> Strategy3 -> Strategy3
bestResponseSeat13 eq o2 weights qs hero opp = go opp (scoreAnalytical3 eq qs hero opp) (0 :: Int)
  where
    go s lastEV iter
      | iter > 20 = s
      | otherwise =
          let s' = foldl' (\s1 ctx -> bestResponseContext3 eq o2 weights qs hero s1 ctx False) s [1, 2, 3]
              ev = scoreAnalytical3 eq qs hero s'
           in if abs (ev - lastEV) < 1e-7 then s' else go s' ev (iter + 1)

-- | Alternating exact best-response dynamics: optimize all SB contexts, then all
-- BB contexts, each as a block.  This is the multi-coordinate step the user
-- wanted — it optimizes every threshold in a seat's contexts simultaneously.
alternatingBestResponse :: EqMatrix -> Range Double -> Range Double -> [Double] -> Strategy3 -> Double -> Int -> Int -> FilePath -> IO (Strategy3, Double)
alternatingBestResponse eq o2 weights qs s ev n limit logFile
  | n > limit = do
      putStrLn $ "Reached alternating-best-response step limit " ++ show limit
      return (s, ev)
  | otherwise = do
      putStrLn $ "\n=== Alternating-best-response step " ++ show n ++ " ==="
      putStrLn $ "Current EV: " ++ show ev
      let sSB = bestResponseSeat03 eq o2 weights qs s s
          evSB = scoreAnalytical3 eq qs sSB s
          sBB = bestResponseSeat13 eq o2 weights qs sSB s
          evBB = scoreAnalytical3 eq qs sSB sBB
      putStrLn $ "After SB block EV: " ++ show evSB
      putStrLn $ "After BB block EV: " ++ show evBB
      if abs (evBB - ev) < 1e-7
        then do
          putStrLn $ "Converged at step " ++ show n
          return (sBB, evBB)
        else do
          logABRStep logFile n ev evBB sBB
          alternatingBestResponse eq o2 weights qs sBB evBB (n + 1) limit logFile

-- | A single threshold coordinate: context index and threshold index.
type Coord = (Int, Int)

allCoords :: [Coord]
allCoords = [(i, j) | i <- [0 .. 5], j <- [0 .. 3]]

-- | Greedy multi-coordinate block for one role.  'maximize' is True for the SB,
-- False for the BB.  We rank role coordinates by gradient magnitude in the
-- beneficial direction, build one meaningful jump for each, then try applying
-- the largest subset that actually improves self-play EV.
greedyBlock :: EqMatrix -> [Double] -> Strategy3 -> Double -> Int -> Bool -> FilePath -> Int -> IO (Strategy3, Double)
greedyBlock eq qs s ev jump maximize logFile stepNum = do
  gs <- evaluate (gradientMatrix eq qs s 0.01)
  let candidates =
        [ ((i, j), g)
        | i <- coords
        , j <- [0 .. 3]
        , let g = gs !! i !! j
        , if maximize then g > 0 else g < 0
        ]
      ranked = List.sortBy (compare `on` negate . abs . snd) candidates
      moves =
        mapMaybe
          (\(c, g) ->
             let dir = if maximize then 1.0 else -1.0
              in fmap (c,) (nextMeaningfulDelta qs s c dir jump))
          ranked
  case moves of
    [] -> return (s, ev)
    _ -> trySubsets moves
  where
    coords = if maximize then [0, 4, 5] else [1, 2, 3]
    trySubsets [] = return (s, ev)
    trySubsets ms = do
      let s' = applyDeltas s ms
      ev' <- evaluate (scoreAnalytical3 eq qs s' s')
      let improves = if maximize then ev' > ev + 1e-9 else ev' < ev - 1e-9
      if improves
        then do
          logMultiStep logFile stepNum ev ev' s' ms
          putStrLn $ "  Accepted " ++ (if maximize then "SB" else "BB") ++ " block of " ++ show (length ms) ++ " moves | EV: " ++ show ev'
          return (s', ev')
        else trySubsets (init ms)

-- | Alternating multi-coordinate block dynamics: try an SB block, then a BB
-- block, each moving every threshold in that role by one hand-class quantile
-- jump.  This is the "take the largest gradients and jump them together"
-- strategy requested by the user.
greedyMulti :: EqMatrix -> [Double] -> Strategy3 -> Double -> Int -> Int -> Int -> FilePath -> IO (Strategy3, Double)
greedyMulti eq qs s ev n limit jump logFile
  | n > limit = do
      putStrLn $ "Reached multi-coordinate step limit " ++ show limit
      return (s, ev)
  | otherwise = do
      putStrLn $ "\n=== Multi-coordinate step " ++ show n ++ " ==="
      putStrLn $ "Current EV: " ++ show ev
      (sSB, evSB) <- greedyBlock eq qs s ev jump True logFile n
      if sSB /= s
        then greedyMulti eq qs sSB evSB (n + 1) limit jump logFile
        else do
          (sBB, evBB) <- greedyBlock eq qs s ev jump False logFile n
          if sBB /= s
            then greedyMulti eq qs sBB evBB (n + 1) limit jump logFile
            else do
              putStrLn $ "No improving SB or BB block found at step " ++ show n ++ ". Stopping."
              return (s, ev)

-- | Apply a delta to one threshold coordinate, keeping the tuple sorted and
-- clamped to [0,1].
perturbStrategy :: Strategy3 -> Coord -> Double -> Strategy3
perturbStrategy (Strategy3 ts) (i, j) delta =
  let (t0, t1, t2, t3) = ts !! i
      xs = case j of
        0 -> [t0 + delta, t1, t2, t3]
        1 -> [t0, t1 + delta, t2, t3]
        2 -> [t0, t1, t2 + delta, t3]
        3 -> [t0, t1, t2, t3 + delta]
        _ -> error "perturbStrategy: bad threshold index"
      ys = map (max 0.0 . min 1.0) xs
      t' = case sort ys of [a, b, c, d] -> (a, b, c, d); _ -> error "perturbStrategy"
   in Strategy3 (take i ts ++ [t'] ++ drop (i + 1) ts)

-- | Apply many coordinate deltas at once, sorting/clamping each context tuple.
applyDeltas :: Strategy3 -> [(Coord, Double)] -> Strategy3
applyDeltas (Strategy3 ts) moves =
  let deltasByCtx = Map.fromListWith (++) [ (i, [(j, d)]) | ((i, j), d) <- moves ]
      addAt js t j = t + sum [ d | (j', d) <- js, j' == j ]
      newTuple i old =
        let js = Map.findWithDefault [] i deltasByCtx
            [a, b, c, d] =
              sort $ map (max 0.0 . min 1.0)
                [ addAt js (threshComponent old 0) 0
                , addAt js (threshComponent old 1) 1
                , addAt js (threshComponent old 2) 2
                , addAt js (threshComponent old 3) 3
                ]
         in (a, b, c, d)
   in Strategy3 [ newTuple i (ts !! i) | i <- [0 .. 5] ]

-- | Extract one component of a threshold tuple.
threshComponent :: Thresh4 -> Int -> Double
threshComponent (t0, t1, t2, t3) j = case j of 0 -> t0; 1 -> t1; 2 -> t2; 3 -> t3; _ -> error "threshComponent"

-- | Finite-difference gradient matrix for every threshold coordinate.
gradientMatrix :: EqMatrix -> [Double] -> Strategy3 -> Double -> [[Double]]
gradientMatrix eq qs s eps =
  let ts = thresholds3 s
      perturbTuple tup j delta =
        let (t0, t1, t2, t3) = tup
            xs = case j of
              0 -> [t0 + delta, t1, t2, t3]
              1 -> [t0, t1 + delta, t2, t3]
              2 -> [t0, t1, t2 + delta, t3]
              3 -> [t0, t1, t2, t3 + delta]
              _ -> error "perturbTuple"
            ys = map (max 0.0 . min 1.0) xs
         in case sort ys of [a, b, c, d] -> (a, b, c, d); _ -> error "perturbTuple"
      plus i j = Strategy3 (take i ts ++ [perturbTuple (ts !! i) j eps] ++ drop (i + 1) ts)
      minus i j = Strategy3 (take i ts ++ [perturbTuple (ts !! i) j (-eps)] ++ drop (i + 1) ts)
      grad i j = (scoreAnalytical3 eq qs (plus i j) (plus i j) - scoreAnalytical3 eq qs (minus i j) (minus i j)) / (2 * eps)
   in [[grad i j | j <- [0 .. 3]] | i <- [0 .. 5]]

-- | Sorted, duplicate-free list of the hand-class quantiles.
uniqueQuantiles :: [Double] -> [Double]
uniqueQuantiles = map head . List.group . sort

-- | Delta that moves the given coordinate across 'n' actual hand-class quantile
-- boundaries.  'dir' is +1 for an upward move and -1 for a downward move.  For
-- C4/C5 offsets the effective threshold (C0_t0 + offset) is used, so the
-- returned delta applies directly to the stored offset.
nextMeaningfulDelta :: [Double] -> Strategy3 -> Coord -> Double -> Int -> Maybe Double
nextMeaningfulDelta qs s (i, j) dir n =
  let ts = thresholds3 s
      c0t0 = threshComponent (ts !! 0) 0
      t = threshComponent (ts !! i) j
      eff = if i `elem` [4, 5] then c0t0 + t else t
      qs' = uniqueQuantiles qs
      targets
        | dir > 0 = drop (n - 1) $ filter (> eff + 1e-12) qs'
        | otherwise = drop (n - 1) $ filter (< eff - 1e-12) (reverse qs')
   in case targets of
        (target : _) -> Just (target - eff)
        _ -> Nothing

-- | Report the self-play EV change from nudging each action boundary one
-- hand-class quantile up or down.  Action boundaries are the sorted threshold
-- values, so the report reflects actual changes to fold/call/raise/all-in
-- bands rather than raw stored threshold coordinates.
sensitivityReport :: EqMatrix -> [Double] -> Strategy3 -> IO ()
sensitivityReport eq qs s = do
  let ev0 = scoreAnalytical3 eq qs s s
      ts = thresholds3 s
      qs' = uniqueQuantiles qs
      boundaryName i k =
        case (i, k) of
          (0, 0) -> "fold/call"; (0, 1) -> "call/raise"; (0, 2) -> "raise/AI"; (0, 3) -> "AI/end"
          (1, 0) -> "fold/call"; (1, 1) -> "call/raise"; (1, 2) -> "raise/AI"; (1, 3) -> "AI/end"
          (2, 0) -> "fold/call"; (2, 1) -> "call/AI";   (2, 2) -> "AI/end";   (2, 3) -> "unused"
          (3, 0) -> "fold/call"; (3, 1) -> "call/end";  (3, 2) -> "unused";   (3, 3) -> "unused"
          (4, 0) -> "fold/call"; (4, 1) -> "call/raise"; (4, 2) -> "raise/AI"; (4, 3) -> "AI/end"
          (5, 0) -> "fold/call"; (5, 1) -> "call/end";  (5, 2) -> "unused";   (5, 3) -> "unused"
          _ -> error "sensitivityReport: bad boundary"
      nudgeBoundary i k dir =
        let tup = ts !! i
            c0t0 = threshComponent (ts !! 0) 0
            bs = case sort [threshComponent tup j | j <- [0 .. 3]] of [a,b,c,d] -> [a,b,c,d]; _ -> error "sensitivityReport"
            -- For C4/C5 the stored values are offsets; the effective action boundary is C0_t0 + offset.
            currentEff = if i `elem` [4, 5] then c0t0 + bs !! k else bs !! k
            targets
              | dir > 0 = filter (> currentEff + 1e-12) qs'
              | otherwise = filter (< currentEff - 1e-12) (reverse qs')
        in case targets of
             (target : _) ->
               let newVal = if i `elem` [4, 5] then target - c0t0 else target
                   bs' = take k bs ++ [newVal] ++ drop (k + 1) bs
                   [a, b, c, d] = sort bs'
                   ts' = take i ts ++ [(a, b, c, d)] ++ drop (i + 1) ts
                in if (a, b, c, d) == (bs !! 0, bs !! 1, bs !! 2, bs !! 3)
                     then Nothing
                     else Just (target - currentEff, Strategy3 ts')
             _ -> Nothing
  putStrLn $ "Baseline self-play EV: " ++ show ev0
  putStrLn ""
  putStrLn "Action-boundary sensitivity (one hand-class quantile nudge):"
  putStrLn "Ctx | boundary  | current   | up-delta  | up-EV     | down-delta | down-EV"
  putStrLn "----|-----------|-----------|-----------|-----------|------------|-----------"
  forM_ [0 .. 5 :: Int] $ \i -> do
    forM_ [0 .. 3 :: Int] $ \k -> do
      let bs = case sort [threshComponent (ts !! i) j | j <- [0 .. 3]] of [a,b,c,d] -> [a,b,c,d]; _ -> error "sensitivityReport"
          current = bs !! k
          mUp = nudgeBoundary i k 1.0
          mDown = nudgeBoundary i k (-1.0)
          fmt Nothing = ("--", "--")
          fmt (Just (d, s')) = (show d, show (scoreAnalytical3 eq qs s' s'))
          (upD, upEv) = fmt mUp
          (downD, downEv) = fmt mDown
      putStrLn $
        "C" ++ show i ++ "  | " ++ pad 9 (boundaryName i k)
          ++ " | " ++ pad 9 (show current)
          ++ " | " ++ pad 9 upD
          ++ " | " ++ pad 9 upEv
          ++ " | " ++ pad 10 downD
          ++ " | " ++ pad 9 downEv

-- | Markdown log of one accepted ascent step.
logStep :: FilePath -> Int -> Coord -> Double -> Double -> Double -> Strategy3 -> [[Double]] -> IO ()
logStep logFile n (i, j) oldVal newVal ev' s gs = do
  let header = "\n\n## Step " ++ show n ++ " — C" ++ show i ++ "_t" ++ show j ++ ": " ++ show oldVal ++ " -> " ++ show newVal ++ "\n\n"
      codeBlock =
        "```haskell\nStrategy3\n  [ "
          ++ List.intercalate ",\n    "
              (zipWith
                (\ctx (t0, t1, t2, t3) ->
                  "("
                    ++ show t0 ++ ", "
                    ++ show t1 ++ ", "
                    ++ show t2 ++ ", "
                    ++ show t3 ++ ") -- C" ++ show ctx)
                [0 .. 5 :: Int]
                (thresholds3 s))
          ++ "\n  ]\n```\n"
      evLine = "\n- **EV (candidate vs candidate):** " ++ show ev' ++ "\n"
      freqLine = "- **Frequencies:** " ++ compactStrategy3 s ++ "\n"
      gradLines =
        "\n- **Gradients (±1% quantile):**\n\n"
          ++ "|     | t0      | t1      | t2      | t3  |\n"
          ++ "|-----|---------|---------|---------|-----|\n"
          ++ unlines
               [ "| C" ++ show i' ++ "  | "
                   ++ unwords [pad 8 (show (gs !! i' !! j')) ++ " |" | j' <- [0 .. 3 :: Int]]
               | i' <- [0 .. 5 :: Int]
               ]
  appendFile logFile (header ++ codeBlock ++ evLine ++ freqLine ++ gradLines)

-- | Markdown log for an alternating-best-response iteration (multi-coordinate step).
logABRStep :: FilePath -> Int -> Double -> Double -> Strategy3 -> IO ()
logABRStep logFile n evOld evNew s = do
  let header = "\n\n## Step " ++ show n ++ " — alternating best-response (SB block + BB block)\n\n"
      codeBlock =
        "```haskell\nStrategy3\n  [ "
          ++ List.intercalate ",\n    "
              (zipWith
                (\ctx (t0, t1, t2, t3) ->
                  "("
                    ++ show t0 ++ ", "
                    ++ show t1 ++ ", "
                    ++ show t2 ++ ", "
                    ++ show t3 ++ ") -- C" ++ show ctx)
                [0 .. 5 :: Int]
                (thresholds3 s))
          ++ "\n  ]\n```\n"
      evLine = "\n- **EV (candidate vs candidate):** " ++ show evNew ++ " (was " ++ show evOld ++ ")\n"
      freqLine = "- **Frequencies:** " ++ compactStrategy3 s ++ "\n"
  appendFile logFile (header ++ codeBlock ++ evLine ++ freqLine)

-- | Markdown log for a greedy multi-coordinate block step.
logMultiStep :: FilePath -> Int -> Double -> Double -> Strategy3 -> [(Coord, Double)] -> IO ()
logMultiStep logFile n evOld evNew s moves = do
  let header = "\n\n## Step " ++ show n ++ " — multi-coordinate block (" ++ show (length moves) ++ " coords)\n\n"
      codeBlock =
        "```haskell\nStrategy3\n  [ "
          ++ List.intercalate ",\n    "
              (zipWith
                (\ctx (t0, t1, t2, t3) ->
                  "("
                    ++ show t0 ++ ", "
                    ++ show t1 ++ ", "
                    ++ show t2 ++ ", "
                    ++ show t3 ++ ") -- C" ++ show ctx)
                [0 .. 5 :: Int]
                (thresholds3 s))
          ++ "\n  ]\n```\n"
      evLine = "\n- **EV (candidate vs candidate):** " ++ show evNew ++ " (was " ++ show evOld ++ ")\n"
      freqLine = "- **Frequencies:** " ++ compactStrategy3 s ++ "\n"
      moveLine = "- **Moves:** " ++ List.intercalate ", " [ "C" ++ show i ++ "_t" ++ show j ++ " " ++ show d | ((i, j), d) <- moves ] ++ "\n"
  appendFile logFile (header ++ codeBlock ++ evLine ++ freqLine ++ moveLine)

-- | True for contexts that belong to the BB and therefore minimise seat-0 EV.
ownerMinimises :: Int -> Bool
ownerMinimises ctx = ctx `elem` [1, 2, 3]

-- | Greedy coordinate ascent: at each step, try the coordinate with the largest
-- |gradient|.  SB coordinates (0,4,5) maximise seat-0 EV; BB coordinates (1,2,3)
-- minimise it.  Stop when a full sweep finds no improving move or the step limit
-- is reached.
coordinateAscent :: EqMatrix -> [Double] -> Strategy3 -> Double -> Int -> Int -> Int -> FilePath -> IO (Strategy3, Double)
coordinateAscent eq qs s ev n limit jump logFile
  | n > limit = do
      putStrLn $ "Reached step limit " ++ show limit
      return (s, ev)
  | otherwise = do
      putStrLn $ "\n=== Step " ++ show n ++ " ==="
      putStrLn $ "Current EV: " ++ show ev
      gs <- evaluate (gradientMatrix eq qs s 0.01)
      let ranked = List.sortBy (compare `on` negate . abs . snd)
                     [(c, val) | (i, row) <- zip [0 .. 5] gs, (j', val) <- zip [0 .. 3] row, let c = (i, j' :: Int), val /= 0.0]
      tryCoords gs ranked
  where
    tryCoords _ [] = do
      putStrLn $ "No improving coordinate found at step " ++ show n ++ ". Stopping."
      return (s, ev)
    tryCoords gs (((i, j), g) : rest) = do
      let target = if ownerMinimises i then -1.0 else 1.0
          sg = signum g
          steps = if sg == 0.0 then [] else maybe [] (: []) (nextMeaningfulDelta qs s (i, j) (target * sg) jump)
      m <- tryDeltas steps (i, j)
      case m of
        Nothing -> tryCoords gs rest
        Just (s', ev', d) -> do
          let oldVal = threshComponent (thresholds3 s !! i) j
              newVal = threshComponent (thresholds3 s' !! i) j
          logStep logFile n (i, j) oldVal newVal ev' s' gs
          putStrLn $ "Accepted C" ++ show i ++ "_t" ++ show j ++ " : " ++ show oldVal ++ " -> " ++ show newVal ++ " (delta " ++ show d ++ ") | EV: " ++ show ev'
          coordinateAscent eq qs s' ev' (n + 1) limit jump logFile
    tryDeltas [] _ = return Nothing
    tryDeltas (d : ds) c@(i, j) = do
      let s' = perturbStrategy s c d
      ev' <- evaluate (scoreAnalytical3 eq qs s' s')
      let improves = if ownerMinimises i then ev' < ev - 1e-9 else ev' > ev + 1e-9
      if improves
        then return (Just (s', ev', d))
        else tryDeltas ds c

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x ys = case break (== x) ys of
  (a, []) -> [a]
  (a, _ : bs) -> a : splitOn x bs

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

readStrategyFile :: FilePath -> IO (String, Int, Strategy3)
readStrategyFile path = do
  content <- readFile path
  let ls = filter (not . null) $ map (trim . takeWhile (/= '#')) $ lines content
      search = fromMaybe "greedy-highest-magnitude" $ listToMaybe $ mapMaybe parseSearch ls
      parseSearch l = trim <$> List.stripPrefix "search:" l
      jump = fromMaybe 1 $ listToMaybe $ mapMaybe parseJump ls
      parseJump l = read . trim <$> List.stripPrefix "jump:" l
      ctxLines = filter (List.isPrefixOf "C") ls
      parseLine l = case break (== ':') l of
        ('C' : n, ':' : rest) -> (read n :: Int, parseTuple rest)
        _ -> error $ "readStrategyFile: cannot parse line: " ++ l
      parseTuple s =
        let ws = splitOn ',' s
         in case map (read . trim) ws of
              [a, b, c, d] -> (a, b, c, d)
              _ -> error $ "readStrategyFile: need four numbers: " ++ s
  return (search, jump, Strategy3 $ map snd $ List.sortOn fst $ map parseLine ctxLines)

writeStrategyFile :: FilePath -> String -> Int -> Strategy3 -> IO ()
writeStrategyFile path search jump (Strategy3 ts) =
  let ls = ("search: " ++ search) : ("jump: " ++ show jump) : zipWith fmt [0 .. 5 :: Int] ts
      fmt i (a, b, c, d) = "C" ++ show i ++ ": " ++ List.intercalate ", " (map show [a, b, c, d])
   in writeFile path (unlines ls)

argValue :: String -> [String] -> Maybe String
argValue _ [] = Nothing
argValue key (k : v : rest) | k == key = Just v
argValue key (_ : rest) = argValue key rest

defaultStrategy :: Strategy3
defaultStrategy =
  Strategy3
    [ (0.43740573152337936, 0.6455505279034698, 0.6455505279034698, 1.0), -- C0 : SB first action
      (0.0, 0.45, 0.4555052790346917, 1.0), -- C1 : BB after SB call
      (0.43740573152337936, 0.6515837104072405, 1.0, 1.0), -- C2 : BB facing SB raise 3bb
      (0.55, 1.0, 1.0, 1.0), -- C3 : BB facing SB all-in
      (5.505279034691668e-3, 0.5, 1.0, 1.0), -- C4 : SB after calling, facing BB raise 3bb
      (0.2, 1.0, 1.0, 1.0) -- C5 : SB after calling, facing BB all-in
    ]

-- | Shift every threshold in a strategy toward 0 (looser, δ < 0) or
-- toward 1 (tighter, δ > 0).  Shift is proportional: t' = t + δ·(1-t)
-- for tightening, t' = t + δ·t for loosening.  Result is clamped and
-- re-sorted per context to maintain ordering.
shiftStrategy :: Double -> Strategy3 -> Strategy3
shiftStrategy delta (Strategy3 ts) =
  Strategy3 [sort4 [clamp (shift (threshComponent tup j)) | j <- [0..3]] | tup <- ts]
  where
    clamp x = max 0.0 (min 1.0 x)
    shift t
      | delta >= 0 = t + delta * (1.0 - t)
      | otherwise  = t * (1.0 + delta)
    sort4 [a,b,c,d] = case Data.List.sort [a,b,c,d] of [x,y,z,w] -> (x,y,z,w); _ -> error "sort4"

-- | Sweep a range of opponent tightness deviations, reporting hero's
-- best-response EV and the raw (unoptimized) hero-vs-opponent EV.
beliefSweep :: EqMatrix -> Range Double -> Range Double -> [Double] -> Strategy3 -> IO ()
beliefSweep eq o2 weights qs baseline = do
  putStrLn "\n=== Belief Sweep ==="
  putStrLn "delta | opp_self_EV | hero_raw_EV | hero_BR_EV | description"
  putStrLn "------|-------------|-------------|------------|-----------"
  let deltas = [-1.0, -0.7, -0.5, -0.3, -0.1, 0.0, 0.1, 0.3, 0.5, 0.7, 1.0]
  forM_ deltas $ \d -> do
    let opp = shiftStrategy d baseline
        oppEv = scoreAnalytical3 eq qs opp opp
        heroRaw = scoreAnalytical3 eq qs baseline opp
        heroBR = bestResponseSeat03 eq o2 weights qs baseline opp
        heroBRev = scoreAnalytical3 eq qs heroBR opp
        desc
          | d <= -0.7 = "very loose"
          | d <= -0.3 = "loose"
          | d < 0      = "slightly loose"
          | d == 0     = "baseline"
          | d < 0.3    = "slightly tight"
          | d < 0.7    = "tight"
          | otherwise  = "very tight"
    putStrLn $ show d ++ " | " ++ show oppEv ++ " | " ++ show heroRaw ++ " | " ++ show heroBRev ++ " | " ++ desc

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  eq <- readEquityMatrix
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"
      weights = ranges Map.! "any2"
      qs = handQuantiles o2 weights

  putStrLn $ "hand quantile sample (weakest / median / strongest): " ++ show (qs !! 0) ++ " / " ++ show (qs !! 84) ++ " / " ++ show (qs !! 168)

  (searchName, jump, candidate) <- case argValue "--candidate" args of
    Just f -> do
      putStrLn $ "\nReading candidate from " ++ f
      readStrategyFile f
    Nothing -> return ("greedy-highest-magnitude", 1, defaultStrategy)

  let ev0 = scoreAnalytical3 eq qs candidate candidate

  let logFile = fromMaybe "results2.md" (argValue "--log-file" args)

  if "--belief-sweep" `elem` args
    then beliefSweep eq o2 weights qs candidate
    else if "--optimize" `elem` args
    then do
      putStrLn $ "\n=== Starting optimization (logs appended to " ++ logFile ++ ") ==="
      putStrLn $ "Search strategy: " ++ searchName
      (finalS, finalEV) <-
        case searchName of
          "alternating-best-response" -> do
            putStrLn "Using exact best-response blocks for SB contexts (0,4,5) and BB contexts (1,2,3)."
            alternatingBestResponse eq o2 weights qs candidate ev0 1 100 logFile
          "greedy-multi" -> do
            putStrLn $ "Using greedy multi-coordinate blocks (jump=" ++ show jump ++ ")."
            greedyMulti eq qs candidate ev0 1 200 jump logFile
          _ -> do
            putStrLn $ "Jump size: " ++ show jump
            coordinateAscent eq qs candidate ev0 1 1000 jump logFile
      let outFile = fromMaybe "candidate-optimized.strat" (argValue "--output" args)
      writeStrategyFile outFile searchName (if searchName == "alternating-best-response" then 0 else jump) finalS
      putStrLn $ "\nFinal EV: " ++ show finalEV
      putStrLn $ "Wrote optimized strategy to " ++ outFile
    else do
      putStrLn "\n=== Candidate strategy (quantile thresholds) ==="
      putStrLn $ "candidate vs candidate: " ++ show ev0
      putStrLn $ compactStrategy3 candidate

      if "--sensitivity" `elem` args
        then sensitivityReport eq qs candidate
        else return ()

      case argValue "--opponent" args of
        Just oppFile -> do
          (_, _, opponent) <- readStrategyFile oppFile
          putStrLn "\nCross-play EV:"
          putStrLn $ "  candidate vs opponent: " ++ show (scoreAnalytical3 eq qs candidate opponent)
          putStrLn $ "  opponent vs candidate: " ++ show (scoreAnalytical3 eq qs opponent candidate)
          putStrLn $ "  opponent vs opponent:  " ++ show (scoreAnalytical3 eq qs opponent opponent)
        Nothing -> return ()

      putStrLn "\nFinite-difference gradients of candidate self-play EV (±1% quantile per threshold):"
      let gs = gradientMatrix eq qs candidate 0.01
      mapM_ (\i -> putStrLn $ "  C" ++ show i ++ ": " ++ unwords [pad 8 (show (gs !! i !! j)) | j <- [0 .. 3 :: Int]]) [0 .. 5 :: Int]

      putStrLn "\n(diagnostics only: EV + gradients)"
