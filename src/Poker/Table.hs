{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Representation of a holdem table
module Poker.Table
  ( -- * Usage
    -- $usage

    -- * Table
    TableCards (..),
    deal,
    SeatState (..),
    Table (..),
    numSeats,
    TableConfig (..),
    defaultTableConfig,
    makeTable,
    liveSeats,
    openSeats,
    nextCursor,
    closed,
    liveHoles,
    hands,
    bbs,

    -- * Betting
    RawAction (..),
    fromRawAction,
    fromRawActionType,
    actOn,

    -- * Showdown
    showdown,
    bestLiveHole,
  )
where

import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.FormatN
import Data.Generics.Labels ()
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector.Storable as S
import GHC.Exts hiding (toList)
import GHC.Generics hiding (from, to)
import Optics.Core
-- import Poker hiding (Card, fromList)
import Poker.Card.Storable
-- import Poker.HandRank.List (cardI)
import Poker.HandRank.Storable
import Prettyprinter hiding (comma)
import Prelude
import Poker.Card (Hole(..), pattern Hole)
import Poker.Card.Iso (cardI)

-- $usage
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> import Control.Monad.State.Lazy
-- >>> import System.Random
-- >>> t = evalState (dealTable defaultTableConfig) (mkStdGen 42)
-- >>> pretty t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> t' = makeTable defaultTableConfig (to cardsS cs)
-- >>> pretty t'
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,

-- $setup
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> import Control.Monad.State.Lazy
-- >>> import System.Random
-- >>> t = evalState (dealTable defaultTableConfig) (mkStdGen 42)
-- >>> pretty t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> t' = makeTable defaultTableConfig (to cardsS cs)
-- >>> pretty t'
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,

-- | Cards dealt to the players and to the table for all streets.
--
-- - each player gets 2 cards. There are typically 2 to 9 players.
--
-- - there are 5 hole cards
--
-- >>> pretty $ cards t
-- Ac7s Tc5s|6d7c6s|9c|4s
data TableCards = TableCards
  { playerCards :: [Hole],
    flopCards :: (Card, Card, Card),
    turnCard :: Card,
    riverCard :: Card
  }
  deriving (Eq, Show, Generic)

{-
instance Pretty TableCards where
  pretty (TableCards ps (f0, f1, f2) t r) =
    concatWith
      (surround "|")
      [ hsep $ (\(Hole x y) -> pretty x <> pretty y) <$> ps,
        pretty f0 <> pretty f1 <> pretty f2,
        pretty t,
        pretty r
      ]

-}

-- | Deal a card list to the table
--
-- FIXME: unwind MkHole
-- >>> deal (to cardsS cs)
-- TableCards {playerCards = [MkHole (Card {rank = Ace, suit = Club}) (Card {rank = Seven, suit = Spade}),MkHole (Card {rank = Ten, suit = Club}) (Card {rank = Five, suit = Spade})], flopCards = (Card {rank = Six, suit = Diamond},Card {rank = Seven, suit = Club},Card {rank = Six, suit = Spade}), turnCard = Card {rank = Nine, suit = Club}, riverCard = Card {rank = Four, suit = Spade}}
deal :: Cards -> TableCards
deal (Cards cs) =
  TableCards
    ( ( \x ->
          MkHole (review cardI $ cs' List.!! (2 * x)) (review cardI $ cs' List.!! (2 * x + 1))
      )
        <$> [0 .. n - 1]
    )
    (cs' List.!! (n * 2), cs' List.!! (1 + n * 2), cs' List.!! (2 + n * 2))
    (cs' List.!! (3 + n * 2))
    (cs' List.!! (4 + n * 2))
  where
    n = (length cs' - 5) `div` 2
    cs' = Card <$> S.toList cs

-- | For each seat, the betting can be open (can re-raise), closed (has called and cannot re-raise). A raise at the table re-opens the betting for all live seats.
--
-- >>> seats t
-- [BettingOpen,BettingOpen]
data SeatState = BettingOpen | BettingClosed | Folded deriving (Eq, Show, Generic)

instance Pretty SeatState where
  pretty BettingOpen = "o"
  pretty BettingClosed = "c"
  pretty Folded = "f"

-- | Table state.
--
-- The structure of a holdem table.
--
-- >>> t = makeTable defaultTableConfig (to cardsS cs)
-- >>> pretty t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
data Table = Table
  { cards :: TableCards,
    -- | position of the next seat to act
    cursor :: Maybe Int,
    seats :: [SeatState],
    stacks :: [Double],
    -- | bets made where seat is in pot.
    bets :: [Double],
    -- | bets from folded seats
    pot :: Double,
    -- | betting history
    history :: [(RawAction, Int)]
  }
  deriving (Eq, Show, Generic)

{-
instance Pretty Table where
  pretty (Table cs n s st bs p h) =
    concatWith
      (surround ",")
      [ pretty cs,
        "hero: " <> pretty n,
        hsep $ pretty <$> s,
        hsep $ pretty . comma (Just 2) <$> st,
        hsep $ pretty . comma (Just 2) <$> bs,
        pretty (comma (Just 2) p),
        concatWith (surround ":") $ (\(a, p) -> pretty a <> pretty p) <$> h
      ]

-}

-- | number of seats at the table
--
-- >>> numSeats t
-- 2
numSeats :: Table -> Int
numSeats = length . seats

-- | list of active player indexes
--
-- >>> liveSeats t
-- [0,1]
liveSeats :: Table -> [Int]
liveSeats t =
  fst
    <$> filter
      ((/= Folded) . snd)
      (zip [0 ..] (seats t))

-- | list of non-cursor actives who can still bet
--
-- >>> openSeats t
-- [1]
openSeats :: Table -> [Int]
openSeats t = case cursor t of
  Nothing -> []
  Just h ->
    fst
      <$> filter
        ((== BettingOpen) . snd)
        (nexts $ zip [0 ..] (seats t))
    where
      nexts l = drop (h + 1) l <> take h l

-- | next seat open to bet
--
-- >>> nextCursor t
-- Just 1
nextCursor :: Table -> Maybe Int
nextCursor t = listToMaybe (openSeats t)

-- | The table is closed when no seat is open, or all but 1 seat has folded.
--
-- >>> closed t
-- False
closed :: Table -> Bool
closed t =
  notElem BettingOpen (seats t)
    || length (filter (/= Folded) (seats t)) <= 1

-- | Index of seat and hands still in the pot
--
-- >>> pretty $ liveHoles t
-- [(0, [Ac, 7s, 6d, 7c, 6s, 9c, 4s]), (1, [Tc, 5s, 6d, 7c, 6s, 9c, 4s])]
liveHoles :: Table -> [(Int, [Card])]
liveHoles t = (\i -> hands (cards t) List.!! i) <$> liveSeats t

-- | Provide the player hands combined with the table cards.
--
-- >>> pretty $ hands $ cards t
-- [(0, [Ac, 7s, 6d, 7c, 6s, 9c, 4s]), (1, [Tc, 5s, 6d, 7c, 6s, 9c, 4s])]
hands :: TableCards -> [(Int, [Card])]
hands (TableCards ps (f0, f1, f2) t r) =
  fmap (fmap (view cardI))
    <$> zip
      [0 .. (length ps - 1)]
      ((\(Hole x y) -> [x, y, review cardI f0, review cardI f1, review cardI f2, review cardI t, review cardI r]) <$> ps)

-- | Static configuration for setting up a table.
--
-- >>> defaultTableConfig
-- TableConfig {tableSize = 2, ante = 0.0, stacks0 = [10.0,10.0]}
data TableConfig = TableConfig
  { tableSize :: Int,
    ante :: Double,
    stacks0 :: [Double]
  }
  deriving (Eq, Show, Generic)

-- | Default is 2 seats, no antes, and equal stacks of 10.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 2 0 (replicate 2 10)

-- | Construct a Table with the supplied cards.
--
-- >>> pretty $ makeTable defaultTableConfig (to cardsS cs)
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
makeTable :: TableConfig -> Cards -> Table
makeTable cfg cs =
  Table (deal cs) (Just 0) (replicate (tableSize cfg) BettingOpen) (zipWith (-) (stacks0 cfg) bs) bs 0 []
  where
    bs = bbs (tableSize cfg) (ante cfg)

-- | standard blind and ante chip structure for n seats.
--
-- >>> bbs 4 1
-- [1.0,1.0,1.5,2.0]
bbs :: Int -> Double -> [Double]
bbs n ante = reverse $ [1 + ante, 0.5 + ante] <> replicate (n - 2) ante

-- | There are three primitive actions that seats *must* perform when they are the cursor:
--
-- Fold
--
-- A seat can fold if there are bets out greater than their current bet, and they have some chips. The Seat becomes Folded, their bet is transferred to the pot and the hand is discarded.
--
-- Call
--
-- A seat bets the lesser of:
--
-- - the difference between the current maximum bet and their current bet, and
-- - the rest of their stack.
--
-- This is called a Call and is the minimum allowed bet action. A Check is when zero is the minimum (maximum bet = their current bet).
--
-- Raise x
--
-- A seat bets x above the difference between the current maximum bet and their current bet, where x <= stack - minimum bet. If x == stack - minimum bet, this is commonly referred to as AllIn.
data RawAction = RawFold | RawCall | RawRaise Double deriving (Eq, Show, Generic)

instance Pretty RawAction where
  pretty RawFold = "f"
  pretty RawCall = "c"
  pretty (RawRaise x) = pretty $ fixed (Just 1) x <> "r"

-- | convert an RawAction to a Char ("f","c", or "r")
--
-- >>> fromRawAction RawFold
-- "f"
fromRawAction :: RawAction -> Text
fromRawAction = fromRawActionType ("f", "c", "r")

-- | Convert from an RawAction to a triple representing fold, call or raise.
--
-- >>> fromRawActionType ("f","c","r") RawFold
-- "f"
fromRawActionType :: (a, a, a) -> RawAction -> a
fromRawActionType (a, _, _) RawFold = a
fromRawActionType (_, a, _) RawCall = a
fromRawActionType (_, _, a) (RawRaise _) = a

-- Is this evil?
update :: Int -> a -> [a] -> [a]
update x a xs = List.take x xs <> [a] <> List.drop (x + 1) xs

adjust :: Int -> (a -> a) -> [a] -> [a]
adjust x f xs = List.take x xs <> [f (xs List.!! x)] <> List.drop (x + 1) xs

-- | A game progresses by seats acting which alters the state of the table.
--
-- >>> pretty t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- The default example is a 2 seat table, stacks of 10 each. The cursor or hero (currently acting seat) is seat 0, Big blind is seat 1. Seat 1 posts the big blind, seat 0 posts the small blind. Both players are open for further bets, stacks are 9.5 and 9.0, bets are 0.5 1.0 and pot is 0
--
-- s0: Restricting the strategy action set to Fold, Call or Raise 10, seat 0 (s0) branches into:
--
-- - s0: Fold
--
-- >>> pretty (actOn RawFold t)
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 1,f o,9.5 9,0 1,0.5,f0
--
-- >>> closed (actOn RawFold t)
-- True
--
-- - s0: Call
--
-- >>> pretty (actOn RawCall t)
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 1,c o,9 9,1 1,0,c0
--
-- s1: s1 is the strategy for seat 1, given betting history of [s0:Call]. They are open for betting (can actOn). They cannot Fold, but can Call or Raise 10
--
--     - s1: Call. At this point, assuming no further betting.
--
-- >>> pretty $ actOn RawCall $ actOn RawCall t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c c,9 9,1 1,0,c1:c0
--
-- Seat 0 wins a small pot.
--
--     - s1: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) $ actOn RawCall t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o c,9 0,1 10,0,9.0r1:c0
--
-- (s2) is the strategy for seat 0, given betting history of [s0:Call, s1:Raise 10]
--
--       - s2: Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) $ actOn RawCall t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,f c,9 0,0 10,1,f0:9.0r1:c0
--
--       - s2: Call
--
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) $ actOn RawCall t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c c,0 0,10 10,0,c0:9.0r1:c0
--
-- Table is closed for betting (cursor == Nothing), and the small blind wins a big pot with a pair of sevens after calling the big blinds allin.
--
-- - s0: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 1,c o,0 9,10 1,0,9.0r0
--
-- (s3) is the strategy for seat 1, given betting history of [s0:Raise 10]
--
--     - s3:Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c f,0 9,10 0,1,f1:9.0r0
--
--     - s3:Call
--
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
actOn :: RawAction -> Table -> Table
actOn RawFold t = case cursor t of
  Nothing -> t
  Just p ->
    -- order of execution matters
    t
      & #bets %~ update p 0
      & #pot %~ (+ (t ^. #bets) List.!! p)
      & #seats
        %~ bool
          (update p BettingClosed)
          (update p Folded)
          -- last player cant fold
          (length (liveSeats t) > 1)
      -- cursor calculation needs to take into account updated seat status
      & (\x -> x & #cursor .~ nextCursor x)
      & #history %~ ((bool RawCall RawFold (length (liveSeats t) > 1), p) :)
actOn RawCall t = case cursor t of
  Nothing -> t
  Just p ->
    t
      & #bets %~ adjust p (+ bet)
      & #stacks %~ adjust p (\x -> x - bet)
      & #seats %~ update p BettingClosed
      & (\t -> t & #cursor .~ nextCursor t)
      & #history %~ ((RawCall, p) :)
    where
      gap = maximum (t ^. #bets) - (t ^. #bets) List.!! p
      st = (t ^. #stacks) List.!! p
      bet = min gap st
actOn (RawRaise r) t = case cursor t of
  Nothing -> t
  Just p ->
    t
      & #bets %~ adjust p (+ bet)
      & #stacks %~ adjust p (\x -> x - bet)
      & #seats %~ update p (bool BettingClosed BettingOpen (st' > 0))
      & ( \x ->
            x
              & bool
                id
                ( #seats
                    .~ zipWith
                      ( \x' st'' ->
                          bool x' BettingOpen (x' == BettingClosed && st'' > 0)
                      )
                      (x ^. #seats)
                      (x ^. #stacks)
                )
                (r' > 0)
        )
      & (\x -> x & #cursor .~ nextCursor x)
      & #history %~ ((bool RawCall (RawRaise r') (r' > 0), p) :)
    where
      gap = maximum (t ^. #bets) - (t ^. #bets) List.!! p
      st = (t ^. #stacks) List.!! p
      bet = min (gap + r) st
      r' = bet - gap
      st' = st - bet

-- | Ship the pot to the winning hands
--
-- >>> pretty $ showdown t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,11 9,0 0,0,
showdown :: Table -> Table
showdown t =
  t
    & #stacks %~ (\s -> foldr ($) s ((\x -> adjust x (+ pot' / fromIntegral (length winners))) <$> winners))
    & #bets .~ fromList (replicate (numSeats t) 0)
    & #pot .~ 0
  where
    pot' = sum (t ^. #bets) + t ^. #pot
    winners = bestLiveHole t

-- | Find the (maybe multiple) best a's
bests :: (Ord a) => [(Int, a)] -> a -> [Int] -> [Int]
bests [] _ res = res
bests ((i, x) : xs) x' res =
  case compare x x' of
    LT -> bests xs x' res
    EQ -> bests xs x' (i : res)
    GT -> bests xs x [i]

-- | index of the winning hands
--
-- >>> bestLiveHole t
-- [0]
bestLiveHole :: Table -> [Int]
bestLiveHole t =
  (\xs -> bests xs 0 [])
    (fmap (second (lookupHRUnsafe . Cards . S.fromList . fmap unwrapCard . List.sort)) (liveHoles t))
