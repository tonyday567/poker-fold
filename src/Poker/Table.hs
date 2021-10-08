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

-- | Representation of a holdem table
module Poker.Table
  ( -- * tables
    TableCards (..),
    deal,
    SeatState (..),
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

    -- * Betting
    RawAction (..),
    fromRawAction,
    fromRawActionType,
    actOn,
  )
where

import Data.Bool
import Data.Foldable
import Data.FormatN
import qualified Data.List as List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (Text)
import GHC.Exts hiding (toList)
import GHC.Generics hiding (from, to)
import Lens.Micro hiding (to)
import Poker hiding (fromList)
import Poker.Card.Storable
import Prettyprinter hiding (comma)
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Prettyprinter
-- >>> import qualified Data.List as List
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> let cs = [Card {rank = Ace, suit = Heart},Card {rank = Seven, suit = Spade},Card {rank = Ten, suit = Heart},Card {rank = Five, suit = Spade},Card {rank = Six, suit = Club},Card {rank = Seven, suit = Heart},Card {rank = Six, suit = Spade},Card {rank = Nine, suit = Heart},Card {rank = Four, suit = Spade}]
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,

-- | A typical poker table setup for texas holdem.
--
-- - each player gets 2 cards. There are typically 2 to 9 players.
--
-- - there are 5 hole cards
--
-- >>> pretty $ deal cs
-- Ah7s Th5s|6c7h6s|9h|4s
data TableCards = TableCards
  { playerCards :: [Hand],
    flopCards :: (Card, Card, Card),
    turnCard :: Card,
    riverCard :: Card
  }
  deriving (Eq, Show, Generic)

instance Pretty TableCards where
  pretty (TableCards ps (f0, f1, f2) t r) =
    concatWith
      (surround "|")
      [ hsep $ (\(Hand x y) -> pretty x <> pretty y) <$> toList ps,
        pretty f0 <> pretty f1 <> pretty f2,
        pretty t,
        pretty r
      ]

-- | Deal table cards
deal :: [Card] -> TableCards
deal cs =
  TableCards
    ( fromList
        ( ( \x ->
              MkHand (cs List.!! (2 * x)) (cs List.!! (2 * x + 1))
          )
            <$> [0 .. n - 1]
        )
    )
    (cs List.!! (n * 2), cs List.!! (1 + n * 2), cs List.!! (2 + n * 2))
    (cs List.!! (3 + n * 2))
    (cs List.!! (4 + n * 2))
  where
    n = (length cs - 5) `div` 2

-- | For each seat, the betting can be open (can re-raise), closed (has called and cannot re-raise). A raise at the table re-opens the betting for all live seats.
--
-- SittingOut would be an extra sum type of you would need in live poker.
data SeatState = BettingOpen | BettingClosed | Folded deriving (Eq, Show, Generic)

instance Pretty SeatState where
  pretty BettingOpen = "o"
  pretty BettingClosed = "c"
  pretty Folded = "f"

-- | Table state.
--
-- hero is poker jargon for a cursor into the next seat to act.
--
-- An alternative structure would be a Player type say, with card pair, Seat, stack, bet, but this seems artificial given likely computations that will be happening.
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,
data Table = Table
  { cards :: TableCards,
    hero :: Maybe Int,
    seats :: Seq.Seq SeatState,
    stacks :: Seq.Seq Double,
    bets :: Seq.Seq Double,
    pot :: Double,
    history :: Seq.Seq (RawAction, Int)
  }
  deriving (Eq, Show, Generic)

-- | number of seats at the table
--
-- >>> numSeats t
-- 2
numSeats :: Table -> Int
numSeats ts = length (ts ^. #seats)

instance Pretty Table where
  pretty (Table cs n s st bs p h) =
    concatWith
      (surround ",")
      [ pretty cs,
        "hero: " <> pretty n,
        hsep $ pretty <$> toList s,
        hsep $ pretty . comma (Just 2) <$> toList st,
        hsep $ pretty . comma (Just 2) <$> toList bs,
        pretty (comma (Just 2) p),
        concatWith (surround ":") $ (\(a, p) -> pretty a <> pretty p) <$> toList h
      ]

-- | list of active player indexes
--
-- >>> liveSeats t
-- [0,1]
liveSeats :: Table -> [Int]
liveSeats ts =
  fst
    <$> filter
      ((/= Folded) . snd)
      (zip [0 ..] (toList $ ts ^. #seats))

-- | list of non-hero actives who can still bet
--
-- >>> openSeats t
-- [1]
openSeats :: Table -> [Int]
openSeats ts = case ts ^. #hero of
  Nothing -> []
  Just h ->
    fst
      <$> filter
        ((== BettingOpen) . snd)
        (nexts $ zip [0 ..] (toList $ ts ^. #seats))
    where
      nexts l = drop (h + 1) l <> take h l

-- | next seat open to bet
--
-- >>> nextHero t
-- Just 1
nextHero :: Table -> Maybe Int
nextHero ts = listToMaybe (openSeats ts)

-- | The table is closed when no seat is open, or all but 1 seat has folded.
--
-- >>> closed t
-- False
closed :: Table -> Bool
closed ts =
  notElem BettingOpen (ts ^. #seats)
    || length (filter (/= Folded) (toList $ ts ^. #seats)) <= 1

-- | Index of seat and hands in the pot
--
-- >>> pretty $ liveHands t
-- [(0, [Ah, 7s, 6c, 7h, 6s, 9h, 4s]), (1, [Th, 5s, 6c, 7h, 6s, 9h, 4s])]
liveHands :: Table -> [(Int, [Card])]
liveHands ts = (\i -> hands (ts ^. #cards) List.!! i) <$> liveSeats ts

-- | Provide the player hands combined with the table cards.
hands :: TableCards -> [(Int, [Card])]
hands (TableCards ps (f0, f1, f2) t r) =
  zip
    [0 .. (length ps - 1)]
    ((\(Hand x y) -> [x, y, f0, f1, f2, t, r]) <$> ps)

-- | Static configuration for setting up a table.
--
-- >>> defaultTableConfig
-- TableConfig {numPlayers = 2, ante = 0.0, stacks0 = fromList [10.0,10.0]}
data TableConfig = TableConfig
  { numPlayers :: Int,
    ante :: Double,
    stacks0 :: Seq.Seq Double
  }
  deriving (Eq, Show, Generic)

-- | Default is 2 seats, no antes, and equal stacks of 10.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 2 0 (Seq.replicate 2 10)

-- | Construct a Table with the supplied cards.
makeTable :: TableConfig -> [Card] -> Table
makeTable cfg cs = Table (deal cs) (Just 0) (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0 Seq.Empty
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | Construct a Table with the supplied cards.
makeTableS :: TableConfig -> CardsS -> Table
makeTableS cfg cs = Table (deal (to cardsS cs)) (Just 0) (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0 Seq.Empty
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | standard blind and ante chip structure for n seats.
bbs :: Int -> Double -> Seq.Seq Double
bbs n ante = Seq.fromList $ reverse $ [1 + ante, 0.5 + ante] <> replicate (n - 2) ante

-- | There are three primitive actions that seats must perform when they are the hero:
--
-- Fold. A seat can only fold if there are bets out above their current bet, and they have some chips. The Seat becomes Folded and the hand is discarded.
--
-- Call. A seat can bet the difference between the maximum bet and their current bet, or the rest of their stack if this is less. This is called a Call and is the minimum allowed bet action. A check is when zero is the minimum.
--
-- Raise x. A seat bets x above the minimum allowed bet, where x <= stack - minimum allowed.
data RawAction = RawFold | RawCall | RawRaise Double deriving (Eq, Show, Generic)

instance Pretty RawAction where
  pretty RawFold = "f"
  pretty RawCall = "c"
  pretty (RawRaise x) = pretty $ fixed (Just 1) x <> "r"

-- | convert an RawAction top "f","c", or "r"
fromRawAction :: RawAction -> Text
fromRawAction = fromRawActionType ("f", "c", "r")

-- | Convert from an RawAction to a triple representing fold, call or raise.
fromRawActionType :: (a, a, a) -> RawAction -> a
fromRawActionType (a, _, _) RawFold = a
fromRawActionType (_, a, _) RawCall = a
fromRawActionType (_, _, a) (RawRaise _) = a

-- | A game progresses by players taking an action, which alters a table state.
--
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- A 2 player table, where stacks start at 10 each, hero is seat 0, Big blind is seat 1. seat 1 posts the big blind, seat 0 posts the small blind. hero, as utg, is first action.
--
-- s0: Restricting the strategy action set to Fold, Call or Raise 10, seat 0 strategy (s0) branches into:
--
-- - s0: Fold
--
-- >>> pretty (actOn RawFold t)
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,f o,9.5 9,0 1,0.5,f0
--
-- >>> closed (actOn RawFold t)
-- True
--
-- - s0: Call
--
-- >>> pretty (actOn RawCall t)
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,c o,9 9,1 1,0,c0
--
-- s1: s1 is the strategy for seat 1, given betting history of [s0:Call]. They are open for betting (can actOn). They can Call or Raise 10
--
--     - s1: Call. At this point, we assume no further betting (this is equivalent to neither player having an advantage post-flop), and resolve the table.
--
-- >>> pretty $ actOn RawCall $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,9 9,1 1,0,c1:c0
--
-- Seat 0 wins a small pot.
--
--     - s1: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o c,9 0,1 10,0,9.0r1:c0
--
-- (s2) is the strategy for seat 0, given betting history of [s0:Call, s1:Raise 10]
--       - s2: Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,f c,9 0,0 10,1,f0:9.0r1:c0
--
--       - s2: Call
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,0 0,10 10,0,c0:9.0r1:c0
--
-- Table is closed for betting (hero == Nothing), and the small blind wins a big pot with a pair of sevens after calling the big blinds allin.
--
-- - s0: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,c o,0 9,10 1,0,9.0r0
--
-- (s3) is the strategy for seat 1, given betting history of [s0:Raise 10]
--
--     - s3:Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c f,0 9,10 0,1,f1:9.0r0
--
--     - s3:Call
--
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
--
-- One of the reasons actOn is separated from apply is that it can change the incoming Action from a strategy, given table conditions. This may be a design flaw that can be ironed out.
actOn :: RawAction -> Table -> Table
actOn RawFold ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    -- order of execution matters
    ts
      & #bets %~ Seq.update p 0
      & #pot %~ (+ Seq.index (ts ^. #bets) p)
      & #seats
        %~ bool
          (Seq.update p BettingClosed)
          (Seq.update p Folded)
          -- last player cant fold
          (length (liveSeats ts) > 1)
      -- hero calculation needs to take into account updated seat status
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((bool RawCall RawFold (length (liveSeats ts) > 1), p) Seq.:<|)
actOn RawCall ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
      & #bets %~ Seq.adjust' (+ bet) p
      & #stacks %~ Seq.adjust' (\x -> x - bet) p
      & #seats %~ Seq.update p BettingClosed
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((RawCall, p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min gap st
actOn (RawRaise r) ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
      & #bets %~ Seq.adjust' (+ bet) p
      & #stacks %~ Seq.adjust' (\x -> x - bet) p
      & #seats %~ Seq.update p (bool BettingClosed BettingOpen (st' > 0))
      & ( \t ->
            t
              & bool
                id
                ( #seats
                    .~ Seq.zipWith
                      ( \x st'' ->
                          bool x BettingOpen (x == BettingClosed && st'' > 0)
                      )
                      (t ^. #seats)
                      (t ^. #stacks)
                )
                (r' > 0)
        )
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((bool RawCall (RawRaise r') (r' > 0), p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min (gap + r) st
      r' = bet - gap
      st' = st - bet
