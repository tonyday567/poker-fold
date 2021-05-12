{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
-- TODO:
--
-- - https://commons.wikimedia.org/wiki/Category:Playing_cards_set_by_Byron_Knoll
--
-- - https://github.com/atinm/poker-eval/
-- 
-- - https://www.codingthewheel.com/archives/poker-hand-evaluator-roundup/#2p2
--
module Poker
  ( -- * Usage

    --
    -- $setup

    -- * IO
    Short (..),

    -- * basic card types
    Rank (..),
    RankS (..),
    rankS,
    RanksS (..),
    Suit (..),
    SuitS (..),
    suitS,
    Card (..),
    deck,
    ranks,
    suits,
    CardS(..),
    cardS,
    ranksSet,
    toCard,
    fromCard,
    toRankS,
    toSuitS,
    toRanks,
    CardsS(..),
    cardsS,
    Cards2S(..),
    cardsS7V,
    cardsS7L,
    applyFlat,
    applyFlatS,

    -- * Hand & Strat
    Hand (..),
    fromPair,
    toPairs,
    toRepPair,
    Strat (..),
    stratText,
    someStrats,
    readSomeStrats,
    writeSomeStrats,
    enumBs,
    handTypeCount,

    -- * Tables
    TableCards (..),
    deal,
    Seat (..),
    Table (..),
    numSeats,
    TableConfig (..),
    defaultTableConfig,
    makeTable,
    makeTableS,
    liveSeats,
    openSeats,
    nextHero,
    closed,
    liveHands,

    -- * Bets
    Action (..),
    fromAction,
    fromActionType,
    actOn,
    always,
    allin,
    bet,
    apply,

    -- * Shuffling
    enum2,
    ishuffle,

    -- * Hand Ranking
    HandRank (..),
    handRank,
    straight,
    flush,
    kind,
    oRankCount,
    rankCount,
    suitRanks,
    handRankS,
    straightS,
    flushS,
    kindS,
    rankCountS,

    -- * Showdown
    showdown,
    bestLiveHand,

    -- * Combinations
    combinations,
    combinationsR,
    binom,
    binomR,
    binomM,
    toLexiPosR,
    toLexiPosRS,
    fromLexiPosR,
    mapHRValue,
    mapValueHR,
    handValues,
    hvsWrite,
    hvs5Write,
    hvs7Write,
    hvs5,
    hvs7,
    allHandRanks,
    allHandRanksV,
    lookupHR,
    lookupHRs,

    -- * Strategy
    topBs,
    ev,
    ev2Strats,
    winHand,
    winOdds,
    rcf,
  )
where

import Data.Functor.Rep
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
-- >>> :set -XTypeApplications
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> import qualified Data.Text as Text
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> pretty cs
-- A♡7♠T♡5♠6♣7♡6♠9♡4♠
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
--
-- >>> (Just m) <- readSomeStrats
-- >>> Map.keys m
-- ["count","freq","o2","o9"]
--
-- >>> s = m Map.! "o2"
--

-- | create some common enumeration/simulation results in Strat shape, from scratch.
someStrats :: Int -> Map.Map Text (Strat Double)
someStrats n = do
  Map.fromList
    [ ("o2", tabulate (\b -> winHand b 2 n)),
      ("o9", tabulate (\b -> winHand b 9 n)),
      ("count", handTypeCount),
      ("freq", (/ sum handTypeCount) <$> handTypeCount)
    ]

-- | write Strat results to file.
--
-- > writeSomeStrats 1000
--
-- n = 100000 is about 5 mins
writeSomeStrats :: Int -> IO ()
writeSomeStrats n = writeFile "other/some.str" (show $ someStrats n)

-- | read Strat map
--
-- >>> (Just m) <- readSomeStrats
-- >>> index (m Map.! "o2") (Suited Jack Ten)
-- 0.5544
readSomeStrats :: IO (Maybe (Map.Map Text (Strat Double)))
readSomeStrats = do
  t <- readFile "other/some.str"
  pure $ readMaybe (unpack t)

-- | Given a B, what is the chance of that player winning against p other players, simulated n times.
--
-- >>> winHand (Paired Two) 2 100
-- 0.63
winHand :: Hand -> Int -> Int -> Double
winHand b p n =
  (/ fromIntegral n) $ sum $ (\x -> bool (0 :: Double) (1 / fromIntegral (length x)) (0 `elem` x)) . bestLiveHand <$> tablesB p b 0 n

-- | Win odds
--
-- Takes about a minute:
--
-- > winOdds 2 1000
--
-- ![odds2 example](other/odds2.svg)
--
-- > winOdds 9 1000
--
-- ![odds9 example](other/odds9.svg)
winOdds :: Int -> Int -> Strat Double
winOdds p n = tabulate (\b -> winHand b p n)

-- | Top x percent of hands, order determined by a Strat Double, for n-seated.
--
-- >>> pretty (bool "." "x" <$> topBs (m Map.! "o2") 0.25 :: Strat Text)
-- x x x x x x x x x x x . .
-- x x x x x x x . . . . . .
-- x x x . . . . . . . . . .
-- x x x x . . . . . . . . .
-- x x x . x . . . . . . . .
-- x x . . . x . . . . . . .
-- x x . . . . x . . . . . .
-- x x . . . . . x . . . . .
-- x x . . . . . . x . . . .
-- x . . . . . . . . x . . .
-- x . . . . . . . . . x . .
-- x . . . . . . . . . . . .
-- x . . . . . . . . . . . .
topBs :: Strat Double -> Double -> Strat Bool
topBs bs x = tabulate (`elem` top)
  where
    sortedBList x = second snd <$> sortOn (Down . fst . snd) (toList (liftR2 (,) (tabulate id) (liftR2 (,) x handTypeCount)))
    (total, as) = second reverse $ foldl' (\(c', xs) (b, c) -> (c' + c, (b, c' + c) : xs)) (0, []) (sortedBList bs)
    cut = x * total
    top = fst <$> List.takeWhile ((< cut) . snd) as

-- | convert an Action top "f","c", or "r"
fromAction :: Action -> Text
fromAction = fromActionType ("f", "c", "r")

-- | Convert from an Action to a triple representing fold, call or raise.
fromActionType :: (a, a, a) -> Action -> a
fromActionType (a, _, _) Fold = a
fromActionType (_, a, _) Call = a
fromActionType (_, _, a) (Raise _) = a

-- | Construct a Strat Action that chooses to (Raise r) for the top x% of hands, or Call for the top y%, and thus Fold for the bottom (1-y)%.
--
-- eg raising with your top 10% and calling with your top 50% (top defined by o2 stats) is
--
-- >>> pretty $ fromAction <$> rcf (m Map.! "o2") 10 0.1 0.5
-- r r r r r c c c c c c c c
-- r r c c c c c c c c c c c
-- r r r c c c c c c c c f f
-- r c c r c c c f f f f f f
-- r c c c r f f f f f f f f
-- r c c c c r f f f f f f f
-- r c c c c f r f f f f f f
-- c c c c c f f r f f f f f
-- c c c c f f f f r f f f f
-- c c c c f f f f f c f f f
-- c c c f f f f f f f c f f
-- c c c f f f f f f f f c f
-- c c c f f f f f f f f f c
rcf :: Strat Double -> Double -> Double -> Double -> Strat Action
rcf s r x y =
  tabulate
    (\b -> bool (bool Fold Call (index (topBs s y) b)) (Raise r) (index (topBs s x) b))

-- | Simulate the expected value of a strategy
--
-- >>> :set -XOverloadedLabels
-- >>> cards = evalState (replicateM 10 (dealN (5 + 2 * 2))) (mkStdGen 42)
-- >>> acts = [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- >>> ts = makeTable (defaultTableConfig & #numPlayers .~ 2) <$> cards
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
-- >>> ev 2 100 [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- Just (-0.29999999999999893)
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
evTables :: Int -> Int -> [Strat Action] -> [Table]
evTables n sims acts =
  showdown . bet acts <$> tables
  where
    cards = evalState (replicateM sims (dealN (5 + 2 * n))) (mkStdGen 42)
    tables = makeTable (defaultTableConfig & #numPlayers .~ n) <$> cards

-- | Simulate the expected value of a 2 seat game, given the 5 decision point cuts of headsup described in 'actOn'.
--
-- aka bug detector.
--
-- The 5 points are:
--
-- - UTG (Raise 10)
--
-- - UTG Call
--
-- - BB (Raise 10) on UTG Call
--
-- - UTG Call on BB (Raise 10)
--
-- - BB Call on UTG Raise
--
-- >>> (Just m) <- readSomeStrats
-- >>> s = m Map.! "o2"
--
-- [0,0,0,0,0] is iso to always Fold
--
-- >>> ev2Strats s 100 [0,0,0,0,0]
-- Just (-0.5)
--
-- [1,_,1,_] is iso to always Raise
--
-- >>> ev2Strats s 100 [1,1,1,1,1]
-- Just (-1.9049999999999994)
--
-- [0,1,0,_,_] is iso to always Call
--
-- >>> ev2Strats s 100 [0,1,0,1,1]
-- Just (-0.19500000000000028)
ev2Strats :: Strat Double -> Int -> [Double] -> Maybe Double
ev2Strats s sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev 2 sims [rcf s 10 s0r s0c, rcf s 10 s1r 1, rcf s 10 0 s2r, rcf s 10 0 s3r]
ev2Strats _ _ _ = Nothing
