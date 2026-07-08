{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Suit-aware equity analytics.
--
-- The standard 169×169 equity matrix averages over all suit combinations.  This
-- module tracks the (hero pattern, villain pattern, shared-suit count) triple
-- so that EV calculations can weight each suit configuration by its exact
-- dealing probability.
module Poker.EquitySuit
  ( HoleSuitPattern (..),
    SuitPattern (..),
    suitPattern,
    suitPatternCounts,
    suitPatternProbs,
    SuitEqMatrix (..),
    buildEquityMatrixSuit,
    readSuitMatrix,
    writeSuitMatrix,
    readSuitEquityMatrix,
    evAnalyticalSuit,
  )
where

import Control.Monad.ST (runST)
import Control.Monad.State.Lazy qualified as Lazy
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Data.Word (Word16)
import Optics.Core (review, view)
import Poker.Card (Card (..), Hole (..), Suit)
import Poker.Card.Storable (CardS (..), CardsS (..), allCardsS, cardI)
import Poker.EquityMatrix (EqMatrix (..), disjointHoles, eqSize, evAnalytical)
import Poker.HandRank (lookupHRUnsorted)
import Poker.Random (dealNWith)
import Poker.Range (Range, StartingHandS (..), fromHole, startingHandI, toHoles)
import Poker.Strategy (Strategy (..))
import System.Random (mkStdGen)
import Prelude

-- | The three possible suit patterns for a two-card holdem hole.
data HoleSuitPattern = PairedS | SuitedS | OffsuitedS
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A suit pattern for a (hero hole, villain hole) pair.
--
-- * 'heroPat' / 'villPat' describe the internal suit structure of each hole.
-- * 'sharedSuits' is the size of the intersection of the two hole suit sets
--   (0, 1, or 2).
data SuitPattern = SuitPattern
  { heroPat :: !HoleSuitPattern,
    villPat :: !HoleSuitPattern,
    sharedSuits :: !Int
  }
  deriving (Eq, Ord, Show)

patternCount :: Int
patternCount = 27

patternIndex :: SuitPattern -> Int
patternIndex (SuitPattern h v s) = fromEnum h * 9 + fromEnum v * 3 + s

holeSuitPattern :: Hole -> HoleSuitPattern
holeSuitPattern (Hole (Card r1 s1) (Card r2 s2))
  | r1 == r2 = PairedS
  | s1 == s2 = SuitedS
  | otherwise = OffsuitedS

holeSuits :: Hole -> [Suit]
holeSuits (Hole (Card _ s1) (Card _ s2)) = List.nub [s1, s2]

-- | Classify a concrete (hero hole, villain hole) pair by suit pattern.
suitPattern :: Hole -> Hole -> SuitPattern
suitPattern h v =
  SuitPattern
    (holeSuitPattern h)
    (holeSuitPattern v)
    (length (filter (`elem` holeSuits h) (holeSuits v)))

-- | Exact counts of suit patterns for a given pair of starting-hand classes.
--
-- The hero hole is canonicalised via 'toRepHole', then all compatible villain
-- holes in class @vi@ are enumerated.  This handles rank-overlap cases
-- (e.g. KQo vs KJo) correctly.
suitPatternCounts :: Int -> Int -> Map SuitPattern Int
suitPatternCounts hi vi =
  Map.fromListWith
    (+)
    [ (suitPattern h v, 1)
      | h <- toHoles (review startingHandI (StartingHandS (fromIntegral hi))),
        v <- toHoles (review startingHandI (StartingHandS (fromIntegral vi))),
        disjointHoles h v
    ]

-- | Exact probabilities of each suit pattern for a given pair of
-- starting-hand classes.
suitPatternProbs :: Int -> Int -> Map SuitPattern Double
suitPatternProbs hi vi =
  let counts = suitPatternCounts hi vi
      total = fromIntegral (sum (Map.elems counts))
   in fmap (\c -> fromIntegral c / total) counts

-- ===========================================================================
-- Suit-aware equity matrix
-- ===========================================================================

-- | A 169 × 169 × 27 equity matrix keyed by (hero class, villain class,
-- suit pattern).  Each cell stores accumulated wins (+ 0.5 for chops) and a
-- sample count, from which equity is derived as
-- @acc / cnt@ (or 0.5 when @cnt == 0@).
data SuitEqMatrix = SuitEqMatrix
  { unSuitEqAcc :: !(S.Vector Double),
    unSuitEqCnt :: !(S.Vector Int)
  }
  deriving (Eq, Show)

cellIndex :: Int -> Int -> SuitPattern -> Int
cellIndex hi vi p = (hi * eqSize + vi) * patternCount + patternIndex p

-- | Build a suit-aware equity matrix by Monte Carlo.  Each deal updates exactly
-- one (hi, vi, suit-pattern) cell.
buildEquityMatrixSuit :: S.Vector Word16 -> Int -> SuitEqMatrix
buildEquityMatrixSuit hvs n = runST $ do
  let sz = eqSize * eqSize * patternCount
  acc <- SM.replicate sz (0 :: Double)
  cnt <- SM.replicate sz (0 :: Int)

  let loop 0 = do
        a <- S.unsafeFreeze acc
        c <- S.unsafeFreeze cnt
        pure $ SuitEqMatrix a c
      loop k = do
        let CardsS cs =
              evalState
                (dealNWith 9 allCardsS)
                (mkStdGen (fromIntegral k * 137))
            heroHole =
              Hole
                (view cardI (CardS (S.unsafeIndex cs 0)))
                (view cardI (CardS (S.unsafeIndex cs 1)))
            villHole =
              Hole
                (view cardI (CardS (S.unsafeIndex cs 2)))
                (view cardI (CardS (S.unsafeIndex cs 3)))
            hi =
              fromEnum . unStartingHandS $
                view startingHandI (fromHole heroHole)
            vi =
              fromEnum . unStartingHandS $
                view startingHandI (fromHole villHole)
            hero7 =
              CardsS $
                S.fromList
                  [ S.unsafeIndex cs 0,
                    S.unsafeIndex cs 1,
                    S.unsafeIndex cs 4,
                    S.unsafeIndex cs 5,
                    S.unsafeIndex cs 6,
                    S.unsafeIndex cs 7,
                    S.unsafeIndex cs 8
                  ]
            vill7 =
              CardsS $
                S.fromList
                  [ S.unsafeIndex cs 2,
                    S.unsafeIndex cs 3,
                    S.unsafeIndex cs 4,
                    S.unsafeIndex cs 5,
                    S.unsafeIndex cs 6,
                    S.unsafeIndex cs 7,
                    S.unsafeIndex cs 8
                  ]
            hr = lookupHRUnsorted hvs hero7
            vr = lookupHRUnsorted hvs vill7
            sp = suitPattern heroHole villHole
            i = cellIndex hi vi sp
        case compare hr vr of
          GT -> SM.modify acc (+ 1) i >> SM.modify cnt (+ 1) i
          EQ -> SM.modify acc (+ 0.5) i >> SM.modify cnt (+ 1) i
          LT -> SM.modify cnt (+ 1) i
        loop (k - 1)

  loop n
  where
    evalState m g = fst (Lazy.runState m g)

-- | Lookup equity for a specific (hi, vi, suit-pattern) cell.
ixSuit :: SuitEqMatrix -> Int -> Int -> SuitPattern -> Double
ixSuit (SuitEqMatrix acc cnt) hi vi p =
  let i = cellIndex hi vi p
      c = S.unsafeIndex cnt i
   in if c == 0 then 0.5 else S.unsafeIndex acc i / fromIntegral c

-- | Write a 'SuitEqMatrix' to a text file.  Each line contains the
-- accumulated win value and the sample count for one cell, in the flat
-- (hero, villain, pattern) ordering used by 'cellIndex'.
writeSuitMatrix :: SuitEqMatrix -> FilePath -> IO ()
writeSuitMatrix (SuitEqMatrix acc cnt) path =
  writeFile path $
    unlines
      [ show a ++ " " ++ show c
        | (a, c) <- zip (S.toList acc) (S.toList cnt)
      ]

-- | Read a 'SuitEqMatrix' written by 'writeSuitMatrix'.
readSuitMatrix :: FilePath -> IO SuitEqMatrix
readSuitMatrix path = do
  content <- readFile path
  let parse line = case words line of
        [a, c] -> (read a, read c)
        _ -> error $ "readSuitMatrix: malformed line: " ++ line
      pairs = map parse (lines content)
      (acc, cnt) = unzip pairs
  pure $ SuitEqMatrix (S.fromList acc) (S.fromList cnt)

-- | Read the default suit-aware equity matrix from
-- @other/eqmatrixSuit.str@.
readSuitEquityMatrix :: IO SuitEqMatrix
readSuitEquityMatrix = readSuitMatrix "other/eqmatrixSuit.str"

-- | Build a class-average 'EqMatrix' from a 'SuitEqMatrix' by weighting each
-- suit pattern by its exact dealing probability.  Patterns with no samples
-- contribute their unconditional 0.5 fallback; build the matrix with enough
-- samples to keep this rare.
weightedEqMatrix :: SuitEqMatrix -> EqMatrix
weightedEqMatrix eqSuit =
  EqMatrix $
    S.generate
      (eqSize * eqSize)
      ( \i ->
          let hi = i `div` eqSize
              vi = i `mod` eqSize
              probs = suitPatternProbs hi vi
           in sum
                [ p * ixSuit eqSuit hi vi sp
                  | (sp, p) <- Map.toList probs
                ]
      )

-- | Suit-aware expected value of a hero strategy against a villain strategy.
--
-- For each (hero class, villain class) the showdown equity is the exact
-- weighted average over suit patterns, using the probabilities from
-- 'suitPatternProbs' and the equities from 'SuitEqMatrix'.
evAnalyticalSuit ::
  SuitEqMatrix ->
  Range Double ->
  Strategy ->
  Strategy ->
  Double
evAnalyticalSuit eqSuit o2 hero opp =
  evAnalytical (weightedEqMatrix eqSuit) o2 hero opp
