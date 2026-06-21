{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Repeated preflop strategy: percentile thresholds, randomization, and
-- particle-filter inference over the opponent's strategy.
module Poker.Strategy
  ( PreflopAction (..),
    toRawAction,
    fromRawAction,
    actionAggression,
    Thresh4,
    Strategy (..),
    optimalBaseline,
    seat0Baseline,
    seat1Baseline,
    randomStrategy,
    decisionContext,
    contextLegalActions,
    intendedAction,
    strategyAction,
    actionDist,
    posteriorActionDist,
    chooseAction,
    playOne,
    evStrategy,
    Obs (..),
    initialParticles,
    updateParticles,
    resampleParticles,
    effectiveSampleSize,
    posteriorMean,
    compactStrategy,
    compactProfile,
  )
where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy (State, evalState, state)
import Data.Function (on)
import Data.Functor.Rep (index)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.FormatN (fixed)
import Data.Text (unpack)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import GHC.Generics (Generic)
import Optics.Core ((^.), view)
import Poker.Card (Hole)
import Poker.Random (dealTable)
import Poker.Range (Range, fromHole, startingHandI)
import Poker.Table (RawAction (..), Table, actOn, bets, cards, cursor, defaultTableConfig, playerCards, showdown, stacks)
import System.Random (RandomGen, split, uniformR)
import Prelude

-- | The ordered preflop actions available in the repeated game.
-- Ordered by aggression: Fold < Call < Raise2 < Raise5 < AllIn.
data PreflopAction
  = Fold
  | Call
  | Raise2
  | Raise5
  | AllIn
  deriving (Eq, Show, Generic, Enum, Ord)

-- | Convert an abstract preflop action to the raw raise size used by the
-- table engine. Raise sizes are absolute total bet sizes in big blinds.
toRawAction :: PreflopAction -> RawAction
toRawAction Fold = RawFold
toRawAction Call = RawCall
toRawAction Raise2 = RawRaise 1.0
toRawAction Raise5 = RawRaise 4.0
toRawAction AllIn = RawRaise 9.0

-- | Classify a raw action back into the abstract set. Any large raise is
-- treated as all-in.
fromRawAction :: RawAction -> PreflopAction
fromRawAction RawFold = Fold
fromRawAction RawCall = Call
fromRawAction (RawRaise r)
  | r <= 2.0 = Raise2
  | r <= 6.0 = Raise5
  | otherwise = AllIn

-- | Aggression level used to clamp an intended action to the legal set.
actionAggression :: PreflopAction -> Int
actionAggression Fold = 0
actionAggression Call = 1
actionAggression Raise2 = 2
actionAggression Raise5 = 3
actionAggression AllIn = 4

-- | Four sorted thresholds in [0,1].
-- Interpretation by context:
--   * First-to-act (context 0): fold < call < raise2 < raise5 < all-in.
--   * Blind check-raise (context 1): call/check < raise2 < raise5 < all-in
--     (the fold threshold is ignored).
--   * Facing a raise (contexts 2 and 3): fold < call < raise2/raise5
--     (raise thresholds are ignored; all-in clamps to call).
type Thresh4 = (Double, Double, Double, Double)

-- | A full strategy is one threshold tuple for each of the 5 preflop decision
-- points inherited from `ev2Ranges`.
newtype Strategy = Strategy
  { thresholds :: [Thresh4]
  }
  deriving (Eq, Show, Generic)

-- | Discovered near-equilibrium baselines for seat 0 and seat 1 in the
-- extended preflop game. Found by alternating best-response random search
-- (1000 candidates, 500 sims per evaluation, 6 iterations). They are not
-- guaranteed to be exact Nash equilibrium, but their mutual EV is small
-- (~0.21 bb/hand for seat 0).
seat0Baseline :: Strategy
seat0Baseline =
  Strategy
    [ (0.19316202015460127, 0.4175118135895647, 0.7256399730675345, 0.959361702035222),
      (7.04619265030656e-2, 0.28993413085415876, 0.39684631040247886, 0.43229044161381003),
      (0.45283463768528853, 0.5876318520830677, 0.5982082556634782, 0.9577910258147595),
      (0.24717183598261216, 0.25315172611454395, 0.5905440902737731, 0.957920492839119),
      (0.255543586564665, 0.6140307818700375, 0.7928568367449089, 0.9245553824830687)
    ]

seat1Baseline :: Strategy
seat1Baseline =
  Strategy
    [ (0.2987686875577833, 0.3228472843122189, 0.6831320999098519, 0.7294369209550036),
      (0.4669749992474559, 0.6288590625117305, 0.8907779200239675, 0.9184662886509574),
      (1.051479483312423e-2, 0.22123417114394772, 0.30942007135140104, 0.8807000130085043),
      (3.553249852730811e-2, 0.28285892652927136, 0.8199909766173406, 0.9554537587456576),
      (0.14705727680215652, 0.269271719877595, 0.42753745882592453, 0.712429100830351)
    ]

-- | Alias for the seat-0 baseline.
optimalBaseline :: Strategy
optimalBaseline = seat0Baseline

-- | Draw a uniformly random valid strategy.
randomStrategy :: (RandomGen g) => g -> Strategy
randomStrategy g = evalState go g
  where
    go =
      Strategy <$> replicateM 5 (do
          xs <- replicateM 4 (state (uniformR (0, 1)))
          case List.sort xs of
            [a, b, c, d] -> pure (a, b, c, d)
            _ -> error "randomStrategy: expected four thresholds")

-- | Map a table state to one of the five preflop decision contexts.
-- Nothing means no decision is required.
decisionContext :: Table -> Maybe Int
decisionContext t = case cursor t of
  Nothing -> Nothing
  Just p ->
    let gap = maximum (t ^. #bets) - ((t ^. #bets) !! p)
     in Just $ case p of
          0 -> if gap > 0 then 2 else 0
          1 -> if gap > 0 then 3 else 1
          _ -> error "decisionContext: only two seats supported"

-- | Legal raw actions for each decision context, assuming standard 10bb stacks.
contextLegalActions :: Int -> [RawAction]
contextLegalActions 0 = [RawFold, RawCall, RawRaise 1.0, RawRaise 4.0, RawRaise 9.0]
contextLegalActions 1 = [RawCall, RawRaise 1.0, RawRaise 4.0, RawRaise 9.0]
contextLegalActions 2 = [RawFold, RawCall]
contextLegalActions 3 = [RawFold, RawCall]
contextLegalActions _ = error "contextLegalActions: unknown context"

-- | Intended action from a threshold tuple and a hand-strength value.
intendedAction :: Thresh4 -> Double -> PreflopAction
intendedAction (t0, t1, t2, t3) e
  | e < t0 = Fold
  | e < t1 = Call
  | e < t2 = Raise2
  | e < t3 = Raise5
  | otherwise = AllIn

-- | Choose the action a strategy prescribes for a hole in a given context,
-- clamped to the legal actions for that context.
strategyAction :: Range Double -> Strategy -> Int -> Hole -> RawAction
strategyAction o2 strat ctx hole =
  let e = index o2 (view startingHandI (fromHole hole))
      legal = contextLegalActions ctx
      intended = intendedAction (thresholds strat !! ctx) e
   in clampAction intended legal

-- | Pick the legal raw action whose aggression level is nearest to the
-- intended action, preferring the lower-aggression action on ties.
clampAction :: PreflopAction -> [RawAction] -> RawAction
clampAction intended legal =
  let a = actionAggression intended
      legalAs = [(actionAggression (fromRawAction r), r) | r <- legal]
   in snd $ List.minimumBy (compare `on` (\(a', _) -> (abs (a' - a), a'))) legalAs

-- | Probability distribution over legal actions for a single particle,
-- using an epsilon-greedy noise model.
actionDist :: Range Double -> Strategy -> Int -> Hole -> Double -> [(PreflopAction, Double)]
actionDist o2 strat ctx hole eps =
  let e = index o2 (view startingHandI (fromHole hole))
      legal = contextLegalActions ctx
      legalAbs = List.nub (fmap fromRawAction legal)
      l = length legalAbs
      intended = intendedAction (thresholds strat !! ctx) e
      intendedRaw = clampAction intended legal
      intendedAbs = fromRawAction intendedRaw
      point = (1 - eps) + eps / fromIntegral l
      spread = eps / fromIntegral l
   in [(a, if a == intendedAbs then point else spread) | a <- legalAbs]

-- | Aggregate action distribution over a weighted set of particles.
posteriorActionDist :: Range Double -> [(Strategy, Double)] -> Int -> Hole -> Double -> [(PreflopAction, Double)]
posteriorActionDist o2 ps ctx hole eps =
  let legal = contextLegalActions ctx
      legalAbs = List.nub (fmap fromRawAction legal)
      total = sum (snd <$> ps)
      dists = [(w, actionDist o2 s ctx hole eps) | (s, w) <- ps]
      prob a = sum [w * fromMaybe 0 (List.lookup a d) | (w, d) <- dists] / total
   in [(a, prob a) | a <- legalAbs]

-- | Choose the action with highest probability under the posterior mixture.
chooseAction :: Range Double -> [(Strategy, Double)] -> Int -> Hole -> Double -> RawAction
chooseAction o2 ps ctx hole eps =
  let probs = posteriorActionDist o2 ps ctx hole eps
      best = fst $ List.maximumBy (compare `on` snd) probs
   in toRawAction best

-- | Play out one hand between a hero strategy (seat 0) and an opponent
-- strategy (seat 1) and return the hero's profit relative to the initial
-- 10bb buy-in (so posting the blinds counts as a cost).
playOne :: S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Table -> Double
playOne hvs o2 hero opp t0 = go t0
  where
    go t = case cursor t of
      Nothing -> stacks (showdown hvs t) !! 0 - 10.0
      Just p ->
        let ctx = fromMaybe (error "playOne: no decision context") (decisionContext t)
            hole = playerCards (cards t) !! p
            raw = strategyAction o2 (if p == 0 then hero else opp) ctx hole
         in go (actOn raw t)

-- | Expected value of a hero strategy against an opponent strategy over n
-- randomly dealt hands.
evStrategy :: (RandomGen g) => S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Int -> State g Double
evStrategy hvs o2 hero opp n = do
  tables <- replicateM n (dealTable defaultTableConfig)
  let profits = playOne hvs o2 hero opp <$> tables
  pure (sum profits / fromIntegral n)

-- | One observed opponent action, including the exact hole cards revealed at
-- showdown.
data Obs = Obs
  { obsContext :: Int,
    obsHole :: Hole,
    obsAction :: RawAction
  }
  deriving (Eq, Show, Generic)

-- | Initialize a particle filter with equal weights.
initialParticles :: (RandomGen g) => Int -> g -> [(Strategy, Double)]
initialParticles n g = zipWith (\g' _ -> (randomStrategy g', w)) (gens n g) [1 .. n]
  where
    w = 1 / fromIntegral n
    gens 0 _ = []
    gens m g' =
      let (g1, g2) = split g'
       in g1 : gens (m - 1) g2

-- | Update particle weights after observing opponent actions.
-- `eps` is the probability the opponent plays a uniformly random legal action
-- instead of their deterministic policy.
updateParticles :: Double -> Range Double -> [Obs] -> [(Strategy, Double)] -> [(Strategy, Double)]
updateParticles eps o2 obss ps0 = normalize $ foldl (updateOne eps o2) ps0 obss

updateOne :: Double -> Range Double -> [(Strategy, Double)] -> Obs -> [(Strategy, Double)]
updateOne eps o2 ps obs =
  let ctx = obsContext obs
      hole = obsHole obs
      observed = fromRawAction (obsAction obs)
      lk s =
        let dist = actionDist o2 s ctx hole eps
         in fromMaybe 0 (List.lookup observed dist)
   in fmap (\(s, w) -> (s, w * lk s)) ps

normalize :: [(Strategy, Double)] -> [(Strategy, Double)]
normalize ps =
  let total = sum (snd <$> ps)
   in if total <= 0
        then fmap (\(s, _) -> (s, 1 / fromIntegral (length ps))) ps
        else fmap (\(s, w) -> (s, w / total)) ps

-- | Effective sample size of a weighted particle set.
effectiveSampleSize :: [(Strategy, Double)] -> Double
effectiveSampleSize ps =
  let ws = snd <$> ps
   in 1 / sum (fmap (\w -> w * w) ws)

-- | Multinomial resampling. Returns equal-weight particles drawn according to
-- the current weights.
resampleParticles :: (RandomGen g) => [(Strategy, Double)] -> State g [(Strategy, Double)]
resampleParticles ps = do
  let n = length ps
      particles = fst <$> ps
      cum = scanl1 (+) (snd <$> ps)
      pick u =
        fst $
          fromMaybe (last (zip particles cum)) $
            List.find (\(_, c) -> c >= u) (zip particles cum)
  samples <- replicateM n (state (uniformR (0, 1)))
  let ps' = [(pick u, 1 / fromIntegral n) | u <- samples]
  pure ps'

-- | Weighted-mean strategy. Each threshold tuple is averaged componentwise and
-- then re-sorted so the result remains a valid strategy. Useful only for
-- diagnostics; do not use it to choose actions.
posteriorMean :: [(Strategy, Double)] -> Strategy
posteriorMean ps =
  Strategy $ fmap meanCtx [0 .. 4]
  where
    ws = snd <$> ps
    total = sum ws
    meanCtx ctx =
      let (as, bs, cs, ds) = unzip4 [(a, b, c, d) | (s, _) <- ps, let (a, b, c, d) = thresholds s !! ctx]
          avg xs = sum (zipWith (*) xs ws) / total
       in case List.sort [avg as, avg bs, avg cs, avg ds] of
            [a', b', c', d'] -> (a', b', c', d')
            _ -> error "posteriorMean: expected four thresholds"

-- | Compact one-line rendering of the active decision contexts.
-- For two-action contexts only the fold frequency is shown (call is implicit).
compactStrategy :: Strategy -> String
compactStrategy (Strategy ts) = unwords $ zipWith fmt [(0 :: Int) .. 3] (take 4 ts)
  where
    pct x = unpack (fixed (Just 0) (x * 100))
    fmt 0 (t0, t1, t2, t3) =
      "C0: " ++ unwords (pct <$> [t0, t1 - t0, t2 - t1, t3 - t2, 1 - t3])
    fmt 1 (_, t1, t2, t3) =
      "C1: " ++ unwords (pct <$> [t1, t2 - t1, t3 - t2, 1 - t3])
    fmt 2 (t0, _, _, _) = "C2: " ++ pct t0
    fmt 3 (t0, _, _, _) = "C3: " ++ pct t0
    fmt _ _ = ""

-- | Compact one-line rendering of the whole game profile:
-- SB0 / SBR for the small blind, BB0 / BBR for the big blind.
compactProfile :: Strategy -> Strategy -> String
compactProfile sb bb =
  let pct x = unpack (fixed (Just 0) (x * 100))
      fmt5 (t0, t1, t2, t3) = unwords (pct <$> [t0, t1 - t0, t2 - t1, t3 - t2, 1 - t3])
      fmt4 (_, t1, t2, t3) = unwords (pct <$> [t1, t2 - t1, t3 - t2, 1 - t3])
      fmt1 (t0, _, _, _) = pct t0
      (sb0, sbr) = (thresholds sb !! 0, thresholds sb !! 2)
      (bb0, bbr) = (thresholds bb !! 1, thresholds bb !! 3)
   in "SB0: " ++ fmt5 sb0 ++ " SBR: " ++ fmt1 sbr ++ " BB0: " ++ fmt4 bb0 ++ " BBR: " ++ fmt1 bbr

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 xs = (fmap (\(a, _, _, _) -> a) xs, fmap (\(_, b, _, _) -> b) xs, fmap (\(_, _, c, _) -> c) xs, fmap (\(_, _, _, d) -> d) xs)
