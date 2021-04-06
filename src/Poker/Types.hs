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
module Poker.Types where

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
import Data.FormatN
import GHC.Base (error)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels

-- | Unicode is used as a short text representation of most poker types
--
-- >>> short Hearts
-- "\9825"
--
-- >>> putStrLn $ short Hearts
-- ♡
--
-- >>> pretty (Card Ace Spades)
-- A♠
--
-- >>> pretties $ (Card King) <$> [Hearts .. Spades]
-- K♡
-- K♣
-- K♢
-- K♠
--
class Short a where
  short :: a -> Text

  pretty :: a -> IO ()
  pretty = putStrLn . short

  pretties :: (Foldable f) => f a -> IO ()
  pretties xs = putStrLn $ Text.intercalate "\n" $ short <$> toList xs

-- | Rank of a Card
--
-- >>> mconcat $ fmap short (sortOn Down [Two .. Ace])
-- "AKQJT98765432"
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Show, Enum, Generic)

instance NFData Rank

instance Short Rank where
  short Two = "2"
  short Three = "3"
  short Four = "4"
  short Five = "5"
  short Six = "6"
  short Seven = "7"
  short Eight = "8"
  short Nine = "9"
  short Ten = "T"
  short Jack = "J"
  short Queen = "Q"
  short King = "K"
  short Ace = "A"

-- | Suit of a Card
--
-- >>> putStrLn $ mconcat $ fmap short [Hearts .. Spades]
-- ♡♣♢♠
data Suit = Hearts | Clubs | Diamonds| Spades deriving (Eq, Show, Ord, Enum, Generic)

instance NFData Suit

-- | see https://decodeunicode.org/en/u+1F0A2
instance Short Suit where
  short Hearts = "\9825"
  short Clubs = "\9827"
  short Diamonds = "\9826"
  short Spades = "\9824"

-- | Card from a standard 52 card pack.
--
-- >>> pretty $ Card Ten Hearts
-- T♡
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show, Generic)

instance NFData Card

instance Enum Card where
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
  toEnum x = let (d,m) = x `divMod` 4 in Card (toEnum d) (toEnum m)

instance Ord Card where
  (<=) c c' = rank c <= rank c'

instance Short Card where
  short (Card r s) = short r <> short s

instance (Functor f, Foldable f) => Short (f Card) where
  short cs = Text.intercalate "" (toList $ short <$> cs)

-- | a standard 52 card deck
--
-- >>> pretty deck
-- 2♡2♣2♢2♠3♡3♣3♢3♠4♡4♣4♢4♠5♡5♣5♢5♠6♡6♣6♢6♠7♡7♣7♢7♠8♡8♣8♢8♠9♡9♣9♢9♠T♡T♣T♢T♠J♡J♣J♢J♠Q♡Q♣Q♢Q♠K♡K♣K♢K♠A♡A♣A♢A♠
--
deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Hearts .. Spades]

-- | Set of ranks in a hand
ranks :: [Card] -> Set.Set Rank
ranks cs = Set.fromDescList $ rank <$> cs

-- | Set of suits in a hand
suits :: [Card] -> Set.Set Suit
suits cs = Set.fromList $ suit <$> cs

-- | Card pairs dealt to a holdem player have identical probability structure to each other. Iso-sets of card pairs map to a dense representation. This representation forms a basis for modelling player actions, in the presence of uncertainty.
--
-- In the transformation, the suit information is forgotten.
--
-- A (Card, Card) can be:
--
-- - a pair (same rank), which can happen 12 ways. (4 suits x 3 suits)
--
-- - offsuited (of different rank), which can happen 24 ways: lo/high x 4 suits x 3 suits
--
-- - suited (of different rank and the same suit), 8 ways (lo/high rank x 4 suits)
--
-- ![count example](other/count.svg)
--
data B = Paired Rank | Suited Rank Rank | Offsuited Rank Rank deriving (Eq, Show, Ord)

instance Enum B where
  fromEnum (Paired p) = fromEnum p * 13 + fromEnum p
  fromEnum (Offsuited r0 r1) = fromEnum r0 * 13 + fromEnum r1
  fromEnum (Suited r0 r1) = fromEnum r0 + fromEnum r1 * 13

  toEnum x = case compare d m of
    EQ -> Paired $ toEnum d
    LT -> Offsuited (toEnum m) (toEnum d)
    GT -> Suited (toEnum d) (toEnum m)
    where
      (d,m) = x `divMod` 13

instance Short B where
  short (Paired p) = short p <> short p
  short (Suited r0 r1) = short r0 <> short r1 <> "s"
  short (Offsuited r0 r1) = short r0 <> short r1 <> "o"

-- | convert from a Card pair to a B
--
-- >>> c2b (Card Ace Hearts, Card Ace Spades)
-- Paired Ace
--
-- Unpaired cards are forced to high low order.
--
-- >>> c2b (Card Two Hearts, Card Ace Spades)
-- Offsuited Ace Two
--
c2b :: (Card, Card) -> B
c2b (Card r s, Card r' s')
  | r==r' = Paired r
  | s==s' = Suited (max r r') (min r r')
  | otherwise = Offsuited (max r r') (min r r')

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (\(x:y:_) -> (xs List.!! x, xs List.!! y)) $ fmap (fmap toEnum) . (\x y -> ishuffle [x,y]) <$> [0..(n-1)] <*> [0..(n-2)]
  where
    n = length xs

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
-- eg
--
-- >>> shuffle 52 (take 52 rvs52) == ishuffle rvs52
--
ishuffle :: [Int] -> [Int]
ishuffle as = go as []
  where
    go [] dealt = reverse dealt
    go (x0 : xs) dealt = go xs (x1:dealt)
      where
        x1 = foldl' (\acc d -> bool acc (acc+one) (d <= acc)) x0 (sort dealt)

-- | Enumeration of the (Card,Card)'s that B represents.
--
-- >>> putStrLn $ Text.intercalate "." $ (\(x,y) -> short x <> short y) <$> btocs (Paired Ace)
-- A♡A♣.A♡A♢.A♡A♠.A♣A♡.A♣A♢.A♣A♠.A♢A♡.A♢A♣.A♢A♠.A♠A♡.A♠A♣.A♠A♢
btocs :: B -> [(Card, Card)]
btocs (Paired r) = bimap (Card r) (Card r) <$> enum2 [Hearts .. Spades]
btocs (Suited r0 r1) =
  ((\s -> (Card r0 s, Card r1 s)) <$> [Hearts .. Spades]) <>
  ((\s -> (Card r1 s, Card r0 s)) <$> [Hearts .. Spades])
btocs (Offsuited r0 r1) =
  (bimap (Card r0) (Card r1) <$>
    enum2 [Hearts .. Spades]) <>
  (bimap (Card r1) (Card r0) <$>
    enum2 [Hearts .. Spades])

-- | a representative pair of cards for a B, choosing Hearts and Spades.
--
-- Always have a good think about this in the realm of raw card simulation.
--
btoc :: B -> (Card, Card)
btoc (Paired r) = (Card r Hearts, Card r Spades)
btoc (Suited r0 r1) = (Card r0 Hearts, Card r1 Hearts)
btoc (Offsuited r0 r1) = (Card r0 Hearts, Card r1 Spades)

-- | A Strat represents an array of a's indexed by B's.
--
-- Here is a chart of the chances of winning given a B, against another player with any 2.
--
-- ![bwin example](other/bwin.svg)
newtype Strat a =
  Strat
  { array :: Array '[169] a
  } deriving (Eq, Show)

instance Functor Strat where
  fmap f (Strat a) = Strat (fmap f a)

instance Data.Distributive.Distributive Strat where
  distribute = distributeRep

instance Representable Strat where
  type Rep Strat = B

  tabulate f = Strat $ tabulate (f . (\(x:_) -> toEnum x))

  index (Strat a) = index a . (:[]) . fromEnum

-- | enumerate (Card, Card) and count the Bs
--
countBs :: Strat Int
countBs = tabulate (\k -> fromMaybe zero $ Map.lookup k (Map.fromListWith (+) ((,1) . c2b <$> enum2 deck)))

-- | A typical poker table setup for texas holdem.
--
-- - each player gets 2 cards. There are typically 2 to 9 players.
--
-- - there are 5 hole cards
--
-- >>> λ> let t = evalState (dealTable 6) (mkStdGen 42)
-- >>> pretty t
-- 7♡6♠,9♡4♠,J♠3♣,6♢Q♣,J♢J♣,2♢5♡:A♡7♠T♡ 5♠ 6♣
data CardState = CardState
  { players :: Seq.Seq (Card, Card),
    hole :: Seq.Seq Card
  } deriving (Eq, Show, Generic)

instance NFData CardState

instance Short CardState where
  short (CardState ps h) =
    Text.intercalate ","
    [ Text.intercalate " " $ (\(x,y) -> short x <> short y) <$> toList ps,
      mconcat $ short <$> toList h
    ]

card0 :: [Card] -> CardState
card0 cs = do
  CardState (fromList ((\x -> (cs List.!! (2*x), cs List.!! (2*x+1))) <$> [0..n-1])) (fromList $ drop (n*2) cs)
  where
    n = (length cs - 5) `div` 2

-- | Provide the player hands combined with the hole card.
hands :: CardState -> [(Int, [Card])]
hands cs =
  zip [0..(length (cs ^. #players) - 1)] ((\(x,y) -> [x,y] <> toList (cs ^. #hole)) <$> toList (cs ^. #players))

data SeatState = BettingOpen | BettingClosed | Folded deriving (Eq, Show, Generic)

instance Short SeatState where
  short BettingOpen = "o"
  short BettingClosed = "c"
  short Folded = "f"

instance NFData SeatState

-- | cards, next to act, player action available, player stacks, live player bets & pot (folded player bets)
data TableState =
  TableState
  { cards :: CardState,
    hero :: Int,
    seats :: Seq.Seq SeatState,
    stacks :: Seq.Seq Double,
    bets :: Seq.Seq Double,
    pot :: Double
  } deriving (Eq, Show, Generic)

numSeats :: TableState -> Int
numSeats ts = length (ts ^. #seats)

instance NFData TableState

instance Short TableState where
  short (TableState cs n s st bs p) =
    Text.intercalate ","
    [ short cs,
      "hero: " <> show n,
      Text.intercalate " " $ short <$> toList s,
      Text.intercalate " " $ comma (Just 2) <$> toList st,
      Text.intercalate " " $ comma (Just 2) <$> toList bs,
      comma (Just 2) p
    ]

data TableConfig = TableConfig
  { numPlayers :: Int,
    ante :: Double,
    stacks0 :: Seq.Seq Double
  } deriving (Eq, Show, Generic)

defaultTableConfig :: Double -> TableConfig
defaultTableConfig x = TableConfig 2 0 (Seq.replicate 2 x)

-- | Construct a TableState with the supplied cards.
--
-- >>> t = table0 (defaultTableConfig 10) (evalState (dealN 9) (mkStdGen 42))
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,r r,9.5 9,0.5 1,0
table0 :: TableConfig -> [Card] -> TableState
table0 cfg cs = TableState (card0 cs) 0 (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | standard structure with antes
bbs :: Int -> Double -> Seq.Seq Double
bbs n ante = Seq.fromList $ reverse $ [1+ante,0.5+ante] <> replicate (n-2) ante

-- | A game progresses by players taking an action, which alters a table state.
--
-- >>> t = table0 (defaultTableConfig 10) (evalState (dealN 9) (mkStdGen 42))
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,o o,9.5 9,0.5 1,0
--
-- A 2 player table, where stacks start at 10 each, hero is seat 0, Big blind is seat 1. seat 1 posts the big blind, seat 0 posts the small blind. hero, as utg, is first action.
--
-- s0: Restricting the strategy action set to Fold, Call or Raise 10, seat 0 strategy (s0) branches into:
--
-- - s0: Fold
--
-- >>> pretty $ resolve $ (progressTable Fold t)
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 1,f o,9.5 10.5,0 0,0
--
-- - s0: Call
--
-- >>> pretty $ progressTable Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 1,c o,9 9,1 1,0
--
-- s1: s1 is the strategy for seat 1, given betting history of [s0:Call]. They are open for betting (can act). They can Call or Raise 10
--
--     - s1: Call. At this point, we assume no further betting (this is equivalent to neither player having an advantage post-flop), and resolve the table.
--
-- >>> pretty $ resolve $ progressTable Call $ progressTable Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,c c,11 9,0 0,0
--
-- Seat 0 wins a small pot without a showdown.
--
--     - s1: Raise 10
--
-- >>> pretty $ progressTable (Raise 10) $ progressTable Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,o o,9 0,1 10,0
--
-- (s2) is the strategy for seat 0, given betting history of [s0:Call, s1:Raise 10]
--       - s2: Fold
--
-- >>> pretty $ resolve $ progressTable Fold $ progressTable (Raise 10) $ progressTable Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 1,f o,9 11,0 0,0
--
--       - s2: Call
-- >>> pretty $ resolve $ progressTable Call $ progressTable (Raise 10) $ progressTable Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 1,c o,20 0,0 0,0
--
-- Seat 0 wins a big pot with a pair of sevens
--
-- - s0: Raise 10
--
-- >>> pretty $ progressTable (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 1,o o,0 9,10 1,0
--
-- (s3) is the strategy for seat 1, given betting history of [s0:Raise 10]
--
--     - s3:Fold
--
-- >>> pretty $ resolve $ progressTable Fold $ progressTable (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,o f,11 9,0 0,0
--
--     - s3:Call
--
-- >>> pretty $ resolve $ progressTable Call $ progressTable (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: 0,o c,20 0,0 0,0
--
progressTable :: Action -> TableState -> TableState
progressTable Fold ts =
    ts &
    #bets %~ Seq.update p 0 &
    #pot %~ (+ Seq.index (ts ^. #bets) p) &
    #seats %~ bool
               (Seq.update p BettingClosed)
               (Seq.update p Folded)
               (length (live ts) > 1) &
    #hero .~ fromMaybe 0 (nextActor ts)
    where
      p = ts ^. #hero
progressTable Call ts =
    ts &
    #bets %~ Seq.adjust' (+bet) p &
    #stacks %~ Seq.adjust' (\x -> x - bet) p &
    #seats %~ Seq.update p BettingClosed &
    #hero .~ fromMaybe 0 (nextActor ts)
    where
      p = ts ^. #hero
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min gap st
progressTable (Raise r) ts =
    ts &
    #bets %~ Seq.adjust' (+bet) p &
    #stacks %~ Seq.adjust' (\x -> x - bet) p &
    bool
     (#seats %~ Seq.update (ts ^. #hero) BettingClosed)
     ((#seats %~ Seq.update (ts ^. #hero) BettingOpen) .
     (#seats %~ fmap (\x -> bool x BettingOpen (x==BettingClosed))))
     (st > gap) &
    (\x -> x & #hero .~ fromMaybe 99 (nextActor x))
    where
      p = ts ^. #hero
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min (gap+r) st

-- | list of active player indexes
live :: TableState -> [Int]
live ts =
  fmap fst $
  filter (not . (==Folded) . snd)
  (zip [0..] (toList $ ts ^. #seats))

-- | list of actives who can still bet
remainingBettors :: TableState -> [Int]
remainingBettors ts = l
  where
    l =
      fmap fst $
      filter ((==BettingOpen) . snd)
      (nexts $ zip [0..] (toList $ ts ^. #seats))
    nexts l = drop (ts ^. #hero + 1) l <> take (ts ^. #hero) l

-- | next player to act
nextActor :: TableState -> Maybe Int
nextActor ts = head (remainingBettors ts)

-- | Betting is over when noone can raise.
bettingOver :: TableState -> Bool
bettingOver ts =
  not (any (==BettingOpen) (ts ^. #seats)) ||
  not (length (filter (not . (==Folded)) (toList $ ts ^. #seats)) > 1)

-- | Can seat i still bet?
canBet :: TableState -> Int -> Bool
canBet ts p = (Seq.index (ts ^. #seats) p == BettingOpen) && (Seq.index (ts ^. #stacks) p > 0) && (Seq.index (ts ^. #bets) p < maximum (ts ^. #bets))

-- | Index of seat and hands in the pot
liveHands :: TableState -> [(Int, [Card])]
liveHands ts = (\i -> hands (ts ^. #cards) List.!! i) <$> live ts

-- | FIXME: resolution of Check and Allin into these actions
--
-- is Check == Call?
-- is Raise stack == Allin?
data Action = Fold | Call | Raise Double deriving (Eq, Show, Generic)

-- | Apply a strategy to a table, supplying the next Action.
act :: Strat Action -> TableState -> Action
act a0 ts = index a0 (c2b (Seq.index (ts ^. #cards . #players) (ts ^. #hero)))

-- | Always perform an action
always :: Action -> Strat Action
always a = tabulate (const a)

-- | Raise to the hero's stack size.
allin :: TableState -> Strat Action
allin ts = tabulate (const (Raise x))
  where
    x = Seq.index (ts ^. #stacks) (ts ^. #hero)
