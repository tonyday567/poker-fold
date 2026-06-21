{-# LANGUAGE OverloadedStrings #-}

-- | Equity calculation for starting hands on a given board.
module Poker.Equity
  ( Board,
    equity,
    equityM,
  )
where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy (State, evalState)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Optics.Core (review)
import Poker.Card (Hole (..))
import Poker.Card.Storable
import Poker.HandRank (lookupHRUnsorted)
import Poker.Random (dealNWith)
import Poker.Range (StartingHand, toRepHole)
import System.Random (RandomGen, mkStdGen)
import Prelude

-- | Visible board cards.
type Board = [CardS]

-- | Monte-Carlo equity of a starting hand against a random hand on the given board.
equity :: S.Vector Word16 -> Board -> StartingHand -> Int -> Double
equity s b sh n =
  let (w, t) = evalState (equityM s b sh n) (mkStdGen 42)
   in (fromIntegral w + fromIntegral t / 2) / fromIntegral n

-- | Stateful version that returns (wins, ties) for a given sample count.
equityM ::
  (RandomGen g) =>
  S.Vector Word16 ->
  Board ->
  StartingHand ->
  Int ->
  State g (Int, Int)
equityM s b sh n = do
  samples <- replicateM n (dealNWith (fromIntegral needed) remaining)
  pure $ foldl' go (0, 0) samples
  where
    Hole h0 h1 = toRepHole sh
    hero = fmap unwrapCardS [review cardI h0, review cardI h1]
    board = fmap unwrapCardS b
    dead = S.fromList (hero ++ board)
    remaining = CardsS $ S.filter (\c -> not (S.elem c dead)) (unwrapCardsS allCardsS)
    needed = 2 + (5 - length b)
    go (w, t) sample =
      let sample' = unwrapCardsS sample
          opp = S.take 2 sample'
          runout = S.drop 2 sample'
          boardV = S.fromList board
          heroV = S.fromList hero
          heroHand = CardsS $ heroV S.++ boardV S.++ runout
          oppHand = CardsS $ opp S.++ boardV S.++ runout
          heroRank = lookupHRUnsorted s heroHand
          oppRank = lookupHRUnsorted s oppHand
       in case compare heroRank oppRank of
            GT -> (w + 1, t)
            EQ -> (w, t + 1)
            LT -> (w, t)
