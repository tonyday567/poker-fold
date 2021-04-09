{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | A poker library.
--
-- TODO: https://commons.wikimedia.org/wiki/Category:Playing_cards_set_by_Byron_Knoll
--
module Poker
  ( -- * Usage

    --
    -- $setup

    -- * Cards
    Short (..),
    Rank (..),
    Suit (..),
    Card (..),
    deck,
    ranks,
    suits,

    -- * The Basis strategy representation
    Basis (..),
    fromPair,
    toPairs,
    toRepPair,
    Strat (..),
    stratText,
    readStrat,
    writeStrat,
    fromMap,

    -- * Tables
    TableCards (..),
    deal,
    Seat (..),
    Table (..),
    numSeats,
    TableConfig (..),
    defaultTableConfig,
    dealTable,
    liveSeats,
    openSeats,
    nextHero,
    closed,
    liveHands,

    -- * Bets
    Action (..),
    actOn,
    always,
    allin,
    bet,
    apply,
    applys,
    fromActionRep,

    -- * Shuffling
    enum2,
    ishuffle,

    -- * Hand rankings
    HandRank (..),
    handRank,
    straight,
    flush,
    kind,

    -- * Showdown
    showdown,
    bestLiveHand,

    -- * Strategy
    countBs,
    topBs,
    ev,
    ev2,
    winBasis,
    winOdds,
    rcf,

  )
where

import Data.Functor.Rep
import qualified Data.List as List
import Lens.Micro
import NumHask.Prelude
import Poker.Random
import Poker.Types

-- $setup
--
-- >>> import Poker
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoImplicitPrelude
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> import qualified Data.Text as Text
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> pretty cs
-- A♡7♠T♡5♠6♣7♡6♠9♡4♠
--
-- >>> t = dealTable defaultTableConfig cs
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,

-- | Given a B, what is the chance of that player winning against p other players, simulated n times.
--
-- >>> winBasis (Paired Two) 2 100
-- 0.55
winBasis :: Basis -> Int -> Int -> Double
winBasis b p n =
  (/ fromIntegral n) $ sum $ (\x -> bool (0 :: Double) (1 / fromIntegral (length x)) (0 `elem` x)) . bestLiveHand <$> ts
  where
    ts = evalState (replicateM n (dealTableBasis (defaultTableConfig & #numPlayers .~ p) 0 b)) (mkStdGen 42)

-- | Win odds
--
-- Takes about a minute:
--
-- > o2 = winOdds 2 1000
--
-- > writeChartSvg "other/o2.svg" $ bChart [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3] o2
--
-- ![o2 example](other/o2.svg)
winOdds :: Int -> Int -> Strat Double
winOdds p n = tabulate (\b -> winBasis b p n)

-- | top x percent of Bs for 2-handed.
--
-- > (Just o2) <- readStrat "other/o2.str" :: IO (Maybe (Strat Double))
-- > topBs o2 0.01
--
-- ![topbs example](other/topbs.svg)
topBs :: Strat Double -> Double -> Strat Bool
topBs bs x = (< cut) <$> ((Strat $ fromList $ snd <$> sortOn fst oBs') :: Strat Double)
  where
    oBs = (\x -> (toEnum x,index countBs (toEnum x))) <$> (snd <$> sortOn (Down . fst) (zip (toList bs) [0..168]) :: [Int]) :: [(Basis,Double)]
    accCount = scanl' (+) 0 (snd <$> oBs)
    oBs' = zip (fst <$> oBs) accCount
    cut = x * 2912.0

-- | The combinatorial count.
countBs :: Strat Double
countBs = tabulate $ \case
  (Paired _) -> 8
  (Suited _ _) -> 12
  (Offsuited _ _) -> 24

-- | Construct a strategy (Raise Call Fold) that chooses to (Raise r) for the top x% of Bs, or Call for the top y%, and thus Fold for the bottom (1-y)%.
--
-- > writeChartSvg "other/rcf.svg" $ bChart [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3] $ fromActionRep 5 [0,1,2,3] <$> rcf 10 0.2 0.8
--
-- ![rcf example](other/rcf.svg)
rcf :: Double -> Double -> Double -> Strat Action
rcf r x y =
  tabulate
    (\b -> bool (Raise r) (bool Call Fold (index (topBs countBs x) b)) (index (topBs countBs y) b))

fromActionRep :: Double -> [a] -> Action -> a
fromActionRep cut ts a =
  case a of
    Fold -> ts List.!! 0
    Call -> ts List.!! 1
    (Raise r) -> bool (ts List.!! 2) (ts List.!! 3) (r>cut)

-- | Simulate the expected value of a strategy
--
-- >>> :set -XOverloadedLabels
-- >>> cards = evalState (replicateM 10 (dealN (5 + 2 * 2))) (mkStdGen 42)
-- >>> acts = [rcf 10 0.2 0.9, rcf 10 0.3 0.9, rcf 10 0.1 0.5, rcf 10 0.6 0.8]
-- >>> ts = dealTable (defaultTableConfig & #numPlayers .~ 2) <$> cards
-- >>> pretties ts
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 9♠A♠ 3♣5♠,K♡T♣9♢9♣2♢,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 4♣5♡ 9♣8♢,6♠J♣4♠Q♣Q♢,hero: Just 0,o o,9.5 9,0.5 1,0,
-- Q♠9♠ Q♣8♠,J♡6♢4♡A♢8♡,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 9♠J♠ 5♢2♠,J♡8♡6♢T♡5♠,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 2♣5♡ Q♡3♡,4♣3♢K♠T♢Q♣,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 5♣K♣ 9♠9♣,9♡6♠2♡3♡4♢,hero: Just 0,o o,9.5 9,0.5 1,0,
-- K♡9♡ T♠Q♠,3♣6♠K♢7♣9♣,hero: Just 0,o o,9.5 9,0.5 1,0,
-- K♣A♡ 4♡6♠,6♡8♣6♣5♠3♡,hero: Just 0,o o,9.5 9,0.5 1,0,
-- K♣Q♢ A♢9♢,A♣4♠2♣K♢8♣,hero: Just 0,o o,9.5 9,0.5 1,0,
--
-- >>> ev 2 100 [rcf 10 0.2 0.9, rcf 10 0.3 0.9, rcf 10 0.1 0.5, rcf 10 0.6 0.8]
-- Just (-0.6449999999999996)
ev :: Int -> Int -> [Strat Action] -> Maybe Double
ev n sims acts =
  head $
  fmap ((+ negate 10) . (/ fromIntegral sims) . sum) $
  List.transpose $
  evs n sims acts

-- | Simulate winnings for each seat.
--
-- > all (==20.0) $ sum <$> evs 2 100 [rcf 1 0 0.9, rcf 1 0.3 0.9, rcf 1 0.1 0.5, rcf 1 0.6 0.8]
-- True
evs :: Int -> Int -> [Strat Action] -> [[Double]]
evs n sims acts = fmap (\x -> toList $ x ^. #stacks) (evTables n sims acts)

-- | Simulate end state of tables given strategies.
--
-- FIXME:
--
-- - folding A7o in sim 1.
--
-- - extra c1 when betting should be closed
--
-- >>> pretties $ evTables 2 5 [rcf 10 0.5 0.8, rcf 10 0.3 0.9, rcf 10 0.1 0.5, rcf 10 0.6 0.8]
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Nothing,c c,20 0,0 0,0,10.0r0:c1:10.0r0
-- 9♠A♠ 3♣5♠,K♡T♣9♢9♣2♢,hero: Nothing,f c,9.5 10.5,0 0,0,c1:f0
-- 4♣5♡ 9♣8♢,6♠J♣4♠Q♣Q♢,hero: Nothing,f o,9.5 10.5,0 0,0,10.0r1:f0
-- Q♠9♠ Q♣8♠,J♡6♢4♡A♢8♡,hero: Nothing,f o,9.5 10.5,0 0,0,10.0r1:f0
-- 9♠J♠ 5♢2♠,J♡8♡6♢T♡5♠,hero: Nothing,f o,9.5 10.5,0 0,0,10.0r1:f0
evTables :: Int -> Int -> [Strat Action] -> [Table]
evTables n sims acts =
          fmap showdown $
            applys acts
              . dealTable (defaultTableConfig & #numPlayers .~ n)
              <$> cards
  where
    cards = evalState (replicateM sims (dealN (5 + 2 * n))) (mkStdGen 42)

-- | FIXME: Simulate the expected value of a 2 seat game, given the 5 decision point cuts of headsup described in 'act'.
--
-- > ev2 100 [0,0,0,0,0]
--
--
ev2 :: Int -> [Double] -> Maybe Double
ev2 sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev 2 sims [rcf 5 s0r s0c, rcf 5 s1r 1, rcf 5 0 s2r, rcf 5 0 s3r]
ev2 _ _ = Nothing

