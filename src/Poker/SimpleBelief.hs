{-# LANGUAGE OverloadedStrings #-}

-- | A lightweight, count-based belief updater.
--
-- This does NOT require seeing the opponent's hole cards. It maintains action
-- counts per context. The prior is encoded as pseudocounts derived from a
-- baseline strategy; every observed action increments the corresponding count.
-- The resulting probabilities are just the observed frequencies (smoothed by
-- the prior).
module Poker.SimpleBelief
  ( Belief (..),
    initialBelief,
    initialBeliefWithStrength,
    updateBelief,
    beliefToStrategy,
    adaptHero,
  )
where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Poker.Strategy
  ( Strategy (..),
    Thresh4,
    contextLegalActions,
    seat0Baseline,
    seat1Baseline,
  )
import Poker.Table (RawAction (..))
import Prelude

-- | Belief is a count vector for each of the four active contexts.
-- The counts are ordered according to the legal actions for that context.
newtype Belief = Belief
  { beliefCounts :: [[Double]]
  }
  deriving (Eq, Show)

-- | Probability extraction from thresholds, aware of which actions are legal
-- in each context.
threshToProbs :: Int -> Thresh4 -> [Double]
threshToProbs ctx (t0, t1, t2, t3) = case ctx of
  -- First-to-act: fold < call < raise2 < raise5 < all-in.
  0 -> [t0, t1 - t0, t2 - t1, t3 - t2, 1 - t3]
  -- Blind check-raise: no fold; call < raise2 < raise5 < all-in.
  1 -> [t1, t2 - t1, t3 - t2, 1 - t3]
  -- Facing a raise: fold < call.
  2 -> [t0, 1 - t0]
  3 -> [t0, 1 - t0]
  -- Unused context; treat like first-to-act for completeness.
  _ -> [t0, t1 - t0, t2 - t1, t3 - t2, 1 - t3]

-- | Reconstruct thresholds from a probability vector. The reverse of
-- 'threshToProbs'.
probsToThresh :: Int -> [Double] -> Thresh4
probsToThresh ctx ps =
  let cum = scanl1 (+) ps
   in case ctx of
        0 -> case cum of [a, b, c, d, _] -> (a, b, c, d); _ -> malformed
        1 -> case cum of [a, b, c, _] -> (0, a, b, c); _ -> malformed
        2 -> case cum of [a, _] -> (a, 1, 1, 1); _ -> malformed
        3 -> case cum of [a, _] -> (a, 1, 1, 1); _ -> malformed
        _ -> case cum of [a, b, c, d, _] -> (a, b, c, d); _ -> malformed
  where
    malformed = error "probsToThresh: malformed probability vector"

-- | Convert counts to probabilities.
countsToProbs :: [Double] -> [Double]
countsToProbs cs =
  let total = sum cs
   in if total <= 0 then cs else fmap (/ total) cs

-- | Start from a strategy, converting its thresholds into prior
-- pseudocounts. `priorStrength` controls how many observations are needed to
-- overwhelm the prior.
initialBeliefWithStrength :: Strategy -> Double -> Belief
initialBeliefWithStrength (Strategy ts) priorStrength =
  Belief $ zipWith (\ctx t -> fmap (* priorStrength) (threshToProbs ctx t)) [0 ..] (take 4 ts)

-- | Default prior strength: 5 pseudocounts per context.
initialBelief :: Strategy -> Belief
initialBelief s = initialBeliefWithStrength s 5

-- | Index of an action in the legal set for a context.
actionIndex :: Int -> RawAction -> Int
actionIndex ctx raw =
  fromMaybe (error "actionIndex: illegal action") $
    List.elemIndex raw (contextLegalActions ctx)

-- | Update belief after observing one opponent action: increment the count for
-- that action by one.
updateBelief :: Belief -> Int -> RawAction -> Belief
updateBelief (Belief cs) ctx raw =
  let i = actionIndex ctx raw
      old = cs !! ctx
      new = take i old ++ [old !! i + 1] ++ drop (i + 1) old
   in Belief $ take ctx cs ++ [new] ++ drop (ctx + 1) cs

-- | Convert the current belief back into a strategy. The fifth tuple is copied
-- from seat1Baseline as padding.
beliefToStrategy :: Belief -> Strategy
beliefToStrategy (Belief cs) =
  Strategy $ zipWith (\ctx c -> probsToThresh ctx (countsToProbs c)) [0 ..] cs ++ [thresholds seat1Baseline !! 4]

-- | A simple heuristic that adjusts seat-0 thresholds based on the estimated
-- opponent tendencies.
--
-- - If opponent is more aggressive in C1, tighten C2 (fold more to re-raises).
-- - If opponent folds more to C0 opens in C3, open wider (fold less in C0).
adaptHero :: Belief -> Strategy
adaptHero belief =
  let opp = beliefToStrategy belief
      c1Aggression (Strategy ts) =
        let probs = threshToProbs 1 (ts !! 1)
            weights = [0, 2, 3, 4]
         in sum (zipWith (*) probs weights)
      baseAgg = c1Aggression seat1Baseline
      estAgg = c1Aggression opp
      c2Adj = 0.5 * (estAgg - baseAgg)
      c3Fold (Strategy ts) =
        case threshToProbs 2 (ts !! 3) of
          (p : _) -> p
          _ -> error "c3Fold: no actions"
      baseFold = c3Fold seat1Baseline
      estFold = c3Fold opp
      c0Adj = 0.5 * (estFold - baseFold)
      ts0 = thresholds seat0Baseline
      (c0t0, c0t1, c0t2, c0t3) = ts0 !! 0
      (c2t0, c2t1, c2t2, c2t3) = ts0 !! 2
      clamp01 x = max 0 (min 1 x)
      newC0 = (clamp01 (c0t0 - c0Adj), c0t1, c0t2, c0t3)
      newC2 = (clamp01 (c2t0 + c2Adj), c2t1, c2t2, c2t3)
   in Strategy [newC0, ts0 !! 1, newC2, ts0 !! 3, ts0 !! 4]
