{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Poker.EquityMatrix
  ( EqMatrix (..),
    eqSize,
    ix,
    buildEquityMatrix,
    readMatrix,
    writeMatrix,
    readEquityMatrix,
    evAnalytical,
    evAnalyticalWith,
    strategyToTheta,
    walkPair,
    walkPairWith,
    contextActionValues,
    ev2Ranges',
    disjointHoles,
  )
where

import Control.Monad.ST (runST)
import Control.Monad.State.Lazy qualified as Lazy
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Data.Word (Word16)
import Optics.Core (review, view, (^.))
import Poker.Card (Hole (..))
import Poker.Card.Storable (CardS (..), CardsS (..), allCardsS, cardI)
import Poker.HandRank (lookupHRUnsorted)
import Poker.Random (dealNWith)
import Poker.Range
import Poker.Strategy
import Poker.Table
import System.Random (mkStdGen)
import Prelude

eqSize :: Int
eqSize = 169

newtype EqMatrix = EqMatrix {unEqMatrix :: S.Vector Double}
  deriving (Eq, Show)

ix :: EqMatrix -> Int -> Int -> Double
ix (EqMatrix v) h vil = S.unsafeIndex v (h * eqSize + vil)
{-# INLINE ix #-}

-- | Build the equity matrix by Monte Carlo: N random 9-card deals
-- (2 hero + 2 villain + 5 board), each drawn without replacement
-- from a full 52-card deck.  Each deal updates exactly one cell.
--
-- N = 10M gives ~350 samples/cell, ~5% relative error.  ~5 seconds.
-- N = 100M gives ~3500 samples/cell, ~1.7% error.  ~50 seconds.
buildEquityMatrix :: S.Vector Word16 -> Int -> EqMatrix
buildEquityMatrix hvs n = runST $ do
  let sz = eqSize * eqSize
  acc <- SM.replicate sz (0 :: Double)
  cnt <- SM.replicate sz (0 :: Int)

  let loop 0 = do
        a <- S.unsafeFreeze acc
        c <- S.unsafeFreeze cnt
        pure $
          EqMatrix $
            S.generate sz $ \i ->
              let cn = S.unsafeIndex c i
               in if cn == 0 then 0.5 else S.unsafeIndex a i / fromIntegral cn
      loop k = do
        -- Deal 9 cards without replacement
        let CardsS cs =
              evalState
                (dealNWith 9 allCardsS)
                (mkStdGen (fromIntegral k * 137))
            hi =
              fromEnum . unStartingHandS $
                view startingHandI
                  ( fromHole
                      ( Hole
                          (view cardI (CardS (S.unsafeIndex cs 0)))
                          (view cardI (CardS (S.unsafeIndex cs 1)))
                      )
                  )
            vi =
              fromEnum . unStartingHandS $
                view startingHandI
                  ( fromHole
                      ( Hole
                          (view cardI (CardS (S.unsafeIndex cs 2)))
                          (view cardI (CardS (S.unsafeIndex cs 3)))
                      )
                  )
            -- 7-card hands: hole + 5 board
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
            i = hi * eqSize + vi
        case compare hr vr of
          GT -> SM.modify acc (+ 1) i >> SM.modify cnt (+ 1) i
          EQ -> SM.modify acc (+ 0.5) i >> SM.modify cnt (+ 1) i
          LT -> SM.modify cnt (+ 1) i
        loop (k - 1)

  loop n
  where
    evalState m g = fst (Lazy.runState m g)

writeMatrix :: EqMatrix -> FilePath -> IO ()
writeMatrix (EqMatrix v) path =
  writeFile path (unlines (map show (S.toList v)))

readMatrix :: FilePath -> IO EqMatrix
readMatrix path = do
  content <- readFile path
  let ds = map read (lines content)
  pure $ EqMatrix (S.fromList ds)

readEquityMatrix :: IO EqMatrix
readEquityMatrix = readMatrix "other/eqmatrix.str"

-- ===========================================================================
-- Analytical EV using the real game tree
-- ===========================================================================

ev2Ranges' :: Range Double -> [Double] -> [Range RawAction]
ev2Ranges' o2 (s0r : s0c : s1r : s2r : s3r : _) =
  [ rcf o2 10 s0r s0c,
    rcf o2 10 s1r 1,
    rcf o2 10 0 s2r,
    rcf o2 10 0 s3r
  ]
ev2Ranges' _ _ = []

strategyToTheta :: Strategy -> [Double]
strategyToTheta (Strategy ts) =
  let (s0r, s0c, _, _) = ts !! 0
      (_, _, s1r, _) = ts !! 1
      (s2r, _, _, _) = ts !! 2
      (s3r, _, _, _) = ts !! 3
   in [s0r, s0c, s1r, s2r, s3r]

-- | Play out one hand for a fixed (hero class, villain class) pair, using the
-- precomputed equity matrix in place of a random board.  The caller supplies
-- the context decider and the action selector, so this works with arbitrary
-- strategy/action models.
walkPairWith ::
  EqMatrix ->
  (Table -> Maybe Int) ->
  (Int -> Int -> Hole -> RawAction) ->
  Int ->
  Int ->
  Double
walkPairWith eq decide choose hi vi = go t0
  where
    heroHole = toRepHole (review startingHandI (StartingHandS (fromIntegral hi)))
    villHole = toRepHole (review startingHandI (StartingHandS (fromIntegral vi)))
    dc = [CardS 0, CardS 1, CardS 2, CardS 3, CardS 4]
    t0 =
      Table
        { cards =
            TableCards
              { playerCards = [heroHole, villHole],
                flopCards = (dc !! 0, dc !! 1, dc !! 2),
                turnCard = dc !! 3,
                riverCard = dc !! 4
              },
          cursor = Just 0,
          seats = [BettingOpen, BettingOpen],
          stacks = [9.5, 9.0],
          bets = [0.5, 1.0],
          pot = 0.0,
          street = Preflop,
          history = []
        }
    payoff t =
      let live = liveSeats t
          pot' = sum (t ^. #bets) + t ^. #pot
       in if null live || length live == 1
            then
              if any (== 0) live
                then (t ^. #stacks) !! 0 + pot' - 10.0
                else (t ^. #stacks) !! 0 - 10.0
            else (t ^. #stacks) !! 0 + pot' * ix eq hi vi - 10.0
    go t = case cursor t of
      Nothing -> payoff t
      Just p ->
        let ctx = fromMaybe (error "walkPairWith: no decision context") (decide t)
            hole = playerCards (cards t) !! p
            raw = choose p ctx hole
         in go (actOn raw t)

-- | Like 'walkPairWith', but when the specified decision context is first
-- reached, evaluate every action in the supplied list instead of using the
-- fixed strategy.  Returns a singleton if the context is never reached.
contextActionValues ::
  EqMatrix ->
  (Table -> Maybe Int) ->
  (Int -> Int -> Hole -> RawAction) ->
  [RawAction] ->
  Int ->
  Int ->
  Int ->
  [Double]
contextActionValues eq decide choose actions ctx hi vi = go False t0
  where
    heroHole = toRepHole (review startingHandI (StartingHandS (fromIntegral hi)))
    villHole = toRepHole (review startingHandI (StartingHandS (fromIntegral vi)))
    dc = [CardS 0, CardS 1, CardS 2, CardS 3, CardS 4]
    t0 =
      Table
        { cards =
            TableCards
              { playerCards = [heroHole, villHole],
                flopCards = (dc !! 0, dc !! 1, dc !! 2),
                turnCard = dc !! 3,
                riverCard = dc !! 4
              },
          cursor = Just 0,
          seats = [BettingOpen, BettingOpen],
          stacks = [9.5, 9.0],
          bets = [0.5, 1.0],
          pot = 0.0,
          street = Preflop,
          history = []
        }
    payoff t =
      let live = liveSeats t
          pot' = sum (t ^. #bets) + t ^. #pot
       in if null live || length live == 1
            then
              if any (== 0) live
                then (t ^. #stacks) !! 0 + pot' - 10.0
                else (t ^. #stacks) !! 0 - 10.0
            else (t ^. #stacks) !! 0 + pot' * ix eq hi vi - 10.0
    go used t = case cursor t of
      Nothing -> [payoff t]
      Just p -> case decide t of
        Nothing -> [payoff t]
        Just c
          | not used && c == ctx ->
              [ head (go True (actOn a t)) | a <- actions ]
          | otherwise ->
              let raw = choose p c (playerCards (cards t) !! p)
               in go used (actOn raw t)

-- | Play out one hand for a fixed (hero class, villain class) pair, using the
-- precomputed equity matrix in place of a random board.  Mirrors 'playOne'
-- from "Poker.Strategy" but resolves showdown via 'EqMatrix' instead of actual
-- board cards.
walkPair ::
  EqMatrix ->
  Range Double ->
  Strategy ->
  Strategy ->
  Int ->
  Int ->
  Double
walkPair eq o2 hero opp =
  walkPairWith eq decisionContext (\p ctx hole -> strategyAction o2 (if p == 0 then hero else opp) ctx hole)

-- | Expected value using a generic context decider and action selector.
evAnalyticalWith ::
  EqMatrix ->
  (Table -> Maybe Int) ->
  (Int -> Int -> Hole -> RawAction) ->
  Double
evAnalyticalWith eq decide choose =
  let heroHoles = fmap (toHoles . review startingHandI . StartingHandS . fromIntegral) [0 .. 168 :: Int]
      villHoles = heroHoles
      totalPairs = 2652.0 * 2450.0
      weight hs vs =
        fromIntegral (length [(h, v) | h <- hs, v <- vs, disjointHoles h v]) / totalPairs
   in sum
        [ let wh = weight hs vs
           in wh * walkPairWith eq decide choose hi vi
        | (hi, hs) <- zip [0 .. 168] heroHoles,
          (vi, vs) <- zip [0 .. 168] villHoles
        ]

-- | Expected value of a hero strategy against a villain strategy, computed
-- analytically by summing over all starting-hand-class pairs weighted by their
-- exact joint dealing probability.  Showdown equities come from 'EqMatrix'.
evAnalytical ::
  EqMatrix ->
  Range Double ->
  Strategy ->
  Strategy ->
  Double
evAnalytical eq o2 hero opp =
  evAnalyticalWith eq decisionContext (\p ctx hole -> strategyAction o2 (if p == 0 then hero else opp) ctx hole)

-- | True when two holes share no card.
disjointHoles :: Hole -> Hole -> Bool
disjointHoles (Hole c1 c2) (Hole c3 c4) =
  c1 `notElem` [c3, c4] && c2 `notElem` [c3, c4]
