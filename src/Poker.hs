{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE FlexibleContexts #-}

-- | TODO:
--
-- https://commons.wikimedia.org/wiki/Category:Playing_cards_set_by_Byron_Knoll
--

module Poker where

import Chart
import NumHask.Prelude
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NumHask.Array.Fixed as A
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Lens.Micro
import Perf hiding (zero)
import qualified Data.Vector as V
import GHC.TypeLits
import qualified NumHask.Array.Shape as Shape
import qualified Control.Scanl as Scan
import Data.Mealy
import qualified Data.List as List
import qualified Prelude as P
import NumHask.Space

import Poker.Types
import Poker.HandRank
import Poker.Charts
import Poker.Random


-- | A game is the progress from a TableState to the resolution of the current hand.
--
-- > sum stacks (game _ t) == sum stacks t
--
game :: [Strat Action] -> TableState -> TableState
game acts ts = loop ts 0
  where
    loop ts' n =
      let t = progressTable (act (acts List.!! n) ts') ts' in
      bool (loop t (n+1)) (shipit t) (bettingOver t)

{- | history of progress through the game

>>> acts = [actionCuts 10 0.2 0.9, actionCuts 10 0.2 0.9, actionCuts 10 0.2 0.9, always (Raise 10)]
>>> cards = evalState (replicateM 100 (dealN (5+2*2))) (mkStdGen 42)
>>> pretties $ snd <$> (gameHistory acts $ table0 (defaultTableConfig 10 & #numPlayers .~ 2) (cards List.!! 9))
K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: 99,c c,0 20,0 0,0
K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: 99,c c,0 0,10 10,0
K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: 0,o c,0 0,10 10,0
K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: 1,o o,0 9,10 1,0
K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: 0,o o,9.5 9,0.5 1,0

>>> fst <$> (gameHistory acts $ table0 (defaultTableConfig 10 & #numPlayers .~ 2) (cards List.!! 9))
[Nothing,Just (Raise 10.0),Just (Raise 10.0),Just (Raise 10.0),Nothing]

-}
gameHistory :: [Strat Action] -> TableState -> [(Maybe Action, TableState)]
gameHistory acts ts = loop ts 0 [(Nothing, ts)]
  where
    loop t n xs =
      let (a,t') = (act (acts List.!! n) t, progressTable (act (acts List.!! n) t) t) in
      bool (loop t' (n+1) ((Just a,t'):xs)) ((Nothing, shipit t'):(Just a,t'):xs) (bettingOver t')

-- | Ship the pot to the winning hands
shipit :: TableState -> TableState
shipit ts =
  ts &
  #stacks %~ (\s -> foldr ($) s (Seq.adjust' (+pot'/fromIntegral (length winners)) <$> winners)) &
  #bets .~ fromList (replicate (numSeats ts) 0) &
  #pot .~ 0
  where
    pot' = sum (ts ^. #bets) + (ts ^. #pot)
    winners = bestLiveHand ts

-- | index of the winning hands
bestLiveHand :: TableState -> [Int]
bestLiveHand ts =
  fromMaybe [] $
  fmap (fmap fst) $
  head $
  List.groupBy (\x y -> (snd x :: Maybe HandRank) == snd y)
  (sortOn (Down . snd) (second handRank <$> liveHands ts))

thisB :: Int -> TableState -> B
thisB n ts = c2b (toList (ts ^. #cards . #players) List.!! n)

-- | best hands of those remaining in the pot
bestHand :: [(Int, [Card])] -> Int
bestHand hs =
  fromMaybe 0 $
  head $
  fmap fst $
  sortOn (Down . handRank . snd) hs

-- | Simulate the expected value of a strategy set
--
-- >>> putStrLn $ Text.intercalate "," $ comma (Just 2) <$> ev 2 10 10000 (replicate 4 (always Call))
-- 0.0192,-0.0192
--
-- >>> ev 2 10 10000 (replicate 10 (always (Raise 10)))
-- [0.19200000000000017,-0.19200000000000017]
--
-- >>> ev 2 10 10000 (replicate 4 (always (Fold)))
-- [9.5,10.5]
--
-- >>> ev 2 10 1000 [actionCuts 10 0.2 0.9, actionCuts 10 0.3 0.9, actionCuts 10 0.1 0.5, actionCuts 10 0.6 0.8]
-- [0.20899999999999963,-0.20899999999999963]
ev :: Int -> Double -> Int -> [Strat Action] -> [Double]
ev n x sims acts =
  fmap (+ negate x) $
  fmap (/fromIntegral sims) $
  fmap sum $
  List.transpose $
  fmap (\x -> toList $ x ^. #stacks) $
  game acts .
  table0 (defaultTableConfig x & #numPlayers .~ n) <$> cards
  where
    cards = evalState (replicateM sims (dealN (5+2*n))) (mkStdGen 42)

ev' :: [Double] -> Double
ev' (s0r:s0c:s1r:s2r:s3r:_)= (\(x:_) -> x) $ ev 2 5 1000 [actionCuts 5 s0r s0c, actionCuts 5 s1r 1, actionCuts 5 0 s2r, actionCuts 5 0 s3r]

-- | determine the winner for a table
--
-- >>> let t = evalState (dealTable 9) (mkStdGen 42)
-- >>> pretty t
-- 7♡6♠,9♡4♠,J♠3♣,6♢Q♣,J♢J♣,2♢5♡,6♡K♡,Q♡9♣,2♡7♣:A♡7♠T♡ 5♠ 6♣
--
-- >>> winner t
-- putStrLn $ (\(x,y) -> y <> ": " <> x) $ bimap short show $ winner t
-- 0: A♡7♠7♡6♣6♠
--
-- first player wins with two pair.
--
winner :: TableState -> Int
winner ts = bestHand (liveHands ts)

-- | Given a B, what is the chance of that player winning against p other players, simulated n times.
--
winB :: B -> Int -> Int -> Double
winB b p n = (/fromIntegral n) $ sum $ bool 0 (1::Double) . (0==) . winner <$> evalState (replicateM n (dealTableB (defaultTableConfig 1 & #numPlayers .~ p) 0 b)) (mkStdGen 42)

-- | ordered list of B's, ordered by headsup odds
--
--
obs :: Int -> Int -> [(B,Double)]
obs p n = sortOn (Down . snd) $ (\x -> (toEnum x, win x)) <$> [0..168]
  where
    win x = winB (toEnum x) p n

-- | Given a B in position 0 headsup, what is the win rate against any two cards?
--
-- >>> writeChartSvg "other/bwin.svg" (bWinChart 1000 1)
--
-- ![bwin example](other/bwin.svg)
bWinChart :: Int -> Int -> ChartSvg
bWinChart n p =
  bChart
  [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]
  (Strat (fromList ((\x -> winB x p n) <$> (toEnum <$> [0..168]))))

-- | compare 2 player (x-axis) to 9 player (y-axis) win rates.
--
-- >>> writeChartSvg "other/compare29.svg" (b2Chart compare29)
--
-- ![compare example](other/compare29.svg)
compare29 :: B -> Point Double
compare29 x = Point (winB x 1 1000) (winB x 8 1000)

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  writeChartSvg "other/count.svg" countChart
  writeChartSvg "other/compare29.svg" (b2Chart compare29)
  writeChartSvg "other/bwin.svg" (bWinChart 1000 1)
  writeChartSvg "other/bs.svg" $ mempty & #chartList .~ [stratText]
  writeChartSvg "other/top10.svg" (bChart [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3] (bool 0 1 <$> best 0.5))

-- | top x percent of Bs for 2-handed.
--
-- >>> writeChartSvg "other/top10.svg" (bChart [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3] (Poker.top 0.5))
--
-- ![top10 example](other/top10.svg)
best :: Double -> Strat Bool
best x = tabulate (`elem` bestBs)
  where
    bestBs = take (floor $ x * 169.0) (fst <$> cannedObs2)

actionCuts :: Double -> Double -> Double -> Strat Action
actionCuts r x y =
  tabulate
  (\b -> bool Fold (bool Call (Raise r) (index (best x) b)) (index (best y) b))

cannedObs2 :: [(B,Double)]
cannedObs2 = [(Paired Ace,0.89),(Paired King,0.861),(Paired Queen,0.8160000000000001),(Paired Jack,0.786),(Paired Ten,0.759),(Paired Nine,0.73),(Paired Eight,0.7020000000000001),(Paired Seven,0.684),(Offsuited Ace King,0.669),(Suited Ace Queen,0.667),(Suited Ace King,0.666),(Offsuited Ace Queen,0.663),(Paired Six,0.662),(Suited Ace Eight,0.661),(Suited Ace Ten,0.661),(Offsuited Ace Eight,0.658),(Offsuited Ace Ten,0.657),(Suited Ace Jack,0.648),(Suited King Queen,0.647),(Offsuited Ace Jack,0.645),(Offsuited King Queen,0.645),(Offsuited Ace Nine,0.644),(Suited Ace Nine,0.644),(Suited Ace Five,0.631),(Suited Ace Seven,0.63),(Offsuited Ace Seven,0.628),(Suited Ace Six,0.628),(Offsuited Ace Five,0.627),(Offsuited Ace Six,0.623),(Paired Five,0.621),(Suited King Ten,0.621),(Offsuited King Ten,0.618),(Suited King Jack,0.618),(Offsuited King Jack,0.616),(Suited King Eight,0.616),(Offsuited King Eight,0.613),(Suited King Nine,0.61),(Offsuited King Nine,0.609),(Suited King Five,0.602),(Suited King Seven,0.602),(Suited Ace Three,0.6),(Offsuited King Seven,0.598),(Suited King Six,0.598),(Offsuited Ace Three,0.597),(Offsuited Ace Four,0.596),(Offsuited King Five,0.596),(Suited Ace Four,0.596),(Suited Queen Jack,0.595),(Offsuited King Six,0.593),(Offsuited Queen Jack,0.588),(Suited Queen Ten,0.586),(Suited Ace Two,0.585),(Offsuited Ace Two,0.583),(Offsuited Queen Ten,0.582),(Suited Queen Nine,0.578),(Paired Four,0.5750000000000001),(Offsuited Queen Nine,0.5740000000000001),(Suited Jack Ten,0.5740000000000001),(Suited King Three,0.5700000000000001),(Suited Jack Nine,0.5690000000000001),(Offsuited Jack Ten,0.5680000000000001),(Offsuited King Three,0.5670000000000001),(Offsuited Jack Nine,0.5630000000000001),(Suited Queen Eight,0.561),(Suited King Two,0.561),(Suited King Four,0.559),(Offsuited King Two,0.558),(Paired Three,0.558),(Offsuited Queen Eight,0.558),(Offsuited King Four,0.556),(Suited Queen Six,0.552),(Suited Queen Seven,0.547),(Offsuited Queen Six,0.544),(Suited Queen Five,0.542),(Offsuited Queen Seven,0.541),(Suited Jack Eight,0.541),(Offsuited Jack Eight,0.54),(Suited Jack Seven,0.534),(Offsuited Queen Five,0.533),(Suited Nine Eight,0.533),(Suited Ten Eight,0.533),(Suited Ten Nine,0.532),(Offsuited Jack Seven,0.529),(Offsuited Nine Eight,0.527),(Offsuited Ten Eight,0.525),(Offsuited Ten Nine,0.523),(Suited Queen Three,0.512),(Paired Two,0.511),(Suited Ten Seven,0.511),(Suited Queen Two,0.509),(Offsuited Queen Three,0.508),(Suited Jack Six,0.506),(Offsuited Queen Two,0.505),(Offsuited Ten Seven,0.505),(Suited Jack Five,0.504),(Suited Queen Four,0.501),(Offsuited Jack Six,0.498),(Offsuited Jack Five,0.497),(Offsuited Queen Four,0.494),(Suited Nine Seven,0.494),(Suited Eight Seven,0.492),(Offsuited Nine Seven,0.488),(Offsuited Eight Seven,0.483),(Suited Ten Five,0.483),(Suited Nine Six,0.481),(Suited Ten Six,0.481),(Offsuited Ten Five,0.47700000000000004),(Offsuited Ten Six,0.47400000000000003),(Offsuited Nine Six,0.47300000000000003),(Suited Jack Four,0.47000000000000003),(Suited Nine Five,0.46900000000000003),(Suited Seven Six,0.466),(Offsuited Jack Four,0.464),(Offsuited Nine Five,0.463),(Suited Jack Two,0.463),(Suited Jack Three,0.463),(Offsuited Jack Three,0.462),(Offsuited Jack Two,0.461),(Offsuited Seven Six,0.458),(Suited Eight Six,0.456),(Suited Seven Five,0.452),(Suited Ten Three,0.452),(Offsuited Seven Five,0.451),(Offsuited Ten Three,0.449),(Offsuited Eight Six,0.446),(Suited Eight Five,0.444),(Suited Ten Four,0.444),(Suited Six Five,0.44),(Suited Nine Three,0.44),(Offsuited Ten Two,0.439),(Offsuited Ten Four,0.439),(Offsuited Six Five,0.439),(Suited Ten Two,0.439),(Offsuited Eight Five,0.437),(Offsuited Nine Three,0.434),(Suited Seven Four,0.425),(Suited Nine Four,0.423),(Offsuited Nine Two,0.421),(Offsuited Seven Four,0.421),(Suited Nine Two,0.421),(Offsuited Nine Four,0.417),(Suited Eight Four,0.41400000000000003),(Suited Six Four,0.41200000000000003),(Offsuited Eight Four,0.41000000000000003),(Offsuited Six Four,0.40700000000000003),(Suited Seven Three,0.40700000000000003),(Suited Eight Three,0.406),(Offsuited Seven Three,0.402),(Offsuited Eight Three,0.402),(Suited Five Four,0.402),(Offsuited Five Four,0.398),(Suited Eight Two,0.391),(Offsuited Eight Two,0.388),(Suited Six Three,0.386),(Offsuited Six Three,0.384),(Suited Five Three,0.38),(Offsuited Five Three,0.378),(Suited Seven Two,0.375),(Offsuited Seven Two,0.372),(Suited Six Two,0.372),(Offsuited Six Two,0.37),(Suited Five Two,0.37),(Offsuited Five Two,0.368),(Suited Four Three,0.353),(Offsuited Four Three,0.34900000000000003),(Suited Three Two,0.337),(Offsuited Three Two,0.334),(Suited Four Two,0.333),(Offsuited Four Two,0.33)]
