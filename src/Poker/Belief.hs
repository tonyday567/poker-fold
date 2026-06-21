{-# LANGUAGE OverloadedStrings #-}

-- | Bayesian belief updates over opponent starting-hand ranges.
module Poker.Belief
  ( updateRange,
    equityRange,
    bluffMass,
    drawMass,
  )
where

import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Functor.Rep (index, tabulate)
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Optics.Core (review, view)
import Poker.Action (Action)
import Poker.Card (Card (..), Hole (..))
import Poker.Card.Storable (cardI)
import Poker.Equity (Board, equity)
import Poker.Policy (InfoSet, Policy, policyProb)
import Poker.Range (Range, StartingHand, startingHandI, toRepHole)
import Prelude

-- | Compute an equity range for the given board.
equityRange :: S.Vector Word16 -> Board -> Int -> Range Double
equityRange s board n =
  tabulate (\sh -> equity s board (review startingHandI sh) n)

-- | Update a prior belief range after observing an opponent action.
updateRange ::
  S.Vector Word16 ->
  Policy ->
  InfoSet ->
  Action ->
  Board ->
  Int ->
  -- ^ equity samples
  Range Double ->
  Range Double
updateRange s pol is action board n prior =
  let val = equityRange s board n
      prob = policyProb pol is val action
      posterior :: Range Double
      posterior = tabulate (\sh -> index prior sh * index prob sh)
      total = sum (toList posterior)
   in if total <= 0
        then prior
        else tabulate (\sh -> index posterior sh / total)

-- | Low-equity mass in a range: a crude bluff-density read.
bluffMass :: Range Double -> Range Double -> Double
bluffMass eqr r =
  let low :: Range Double
      low = tabulate (\sh -> bool 0 1 (index eqr sh < 0.35))
      mass :: Range Double
      mass = tabulate (\sh -> index r sh * index low sh)
   in sum (toList mass)

-- | Flush-draw mass in a range.
-- A starting hand is counted as a flush draw if hand + board has exactly 4 cards
-- of some suit (and therefore could complete to a flush on the next card).
drawMass :: Board -> Range Double -> Double
drawMass board r =
  let fd :: Range Double
      fd = tabulate (\sh -> bool 0 1 (isFlushDraw board (review startingHandI sh)))
      mass :: Range Double
      mass = tabulate (\sh -> index r sh * index fd sh)
   in sum (toList mass)

isFlushDraw :: Board -> StartingHand -> Bool
isFlushDraw board sh =
  any (\(_, n) -> n == 4) (Map.toList counts)
  where
    counts =
      Map.fromListWith (+) $
        [(suit c, 1 :: Int) | c <- boardCards] ++ [(suit c, 1) | c <- holeCards]
    boardCards = fmap (view cardI) board
    holeCards =
      let Hole c0 c1 = toRepHole sh
       in [c0, c1]
