{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Poker API.
{-# LANGUAGE DeriveFoldable #-}
module Poker.Types
  ( Short (..),
    Rank (..),
    Suit (..),
    Card (..),
    deck,
    ranks,
    suits,
    Basis (..),
    fromPair,
    toPairs,
    toRepPair,
    Strat (..),
    stratText,
    stratSym,
    readStrat,
    writeStrat,
    fromMap,
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

    -- * Betting
    Action (..),
    actOn,
    always,
    allin,
    bet,
    apply,
    applys,

    -- * Shuffling
    enum2,
    ishuffle,

    -- * Hand Ranking
    HandRank (..),
    handRank,
    straight,
    flush,
    kind,

    -- * Showdown
    bestLiveHand,
    showdown,

  )
where

import Data.Distributive (Distributive (..))
import Data.FormatN
import Data.Functor.Rep
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Lens.Micro
import NumHask.Array.Fixed as A
import NumHask.Prelude
import GHC.Base (error)
import qualified Data.Map.Strict as Map
import qualified Prelude as P

-- $setup
--
-- >>> import Poker.Types
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XRebindableSyntax
-- >>> let cs = [Card {rank = Ace, suit = Hearts},Card {rank = Seven, suit = Spades},Card {rank = Ten, suit = Hearts},Card {rank = Five, suit = Spades},Card {rank = Six, suit = Clubs},Card {rank = Seven, suit = Hearts},Card {rank = Six, suit = Spades},Card {rank = Nine, suit = Hearts},Card {rank = Four, suit = Spades}]
-- >>> t = dealTable defaultTableConfig cs
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
--
-- Hero raises and quick fold from the BB.
--

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
data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
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
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Show, Ord, Enum, Generic)

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
data Card = Card
  { rank :: Rank,
    suit :: Suit
  }
  deriving (Eq, Show, Generic)

instance NFData Card

instance Enum Card where
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
  toEnum x = let (d, m) = x `divMod` 4 in Card (toEnum d) (toEnum m)

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
deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Hearts .. Spades]

-- | Set of ranks in a hand
ranks :: [Card] -> Set.Set Rank
ranks cs = Set.fromDescList $ rank <$> cs

-- | Set of suits in a hand
suits :: [Card] -> Set.Set Suit
suits cs = Set.fromList $ suit <$> cs

-- | Card pairs dealt to a holdem player have identical probability structures to each other, when viewed through a stochastic future of betting action.
--
-- These groupings or iso-sets of card pairs map to a dense representation of what a hand should do, given an element of this iso-set is in their hands. This representation forms a basis for modelling player actions, in the presence of uncertainty about what other seats hold, and what they will do about it.
--
-- A (Card, Card) can be:
--
-- - a pair (the same rank): in 12 enumerations of a deck. (4 suits x 3 suits)
--
-- - offsuited (of different rank): in 24 enumerations of a deck: (low | high ranked) x 4 suits x 3 suits
--
-- - suited (of different rank and the same suit): in 8 enumerations of a deck ((low | high ranked) x 4 suits)
--
-- ![count example](other/count.svg)
--
-- The transformation from (Card, Card) to Basis reduces the strategy vector size from 52*51 to 169, and provides expansion back into the (Card, Card) domain when needed.
--
data Basis =
  Paired Rank |
  Suited Rank Rank |
  Offsuited Rank Rank
  deriving (Eq, Show, Ord)

instance Enum Basis where
  fromEnum (Paired p) = fromEnum p * 13 + fromEnum p
  fromEnum (Offsuited r0 r1) = fromEnum r0 * 13 + fromEnum r1
  fromEnum (Suited r0 r1) = fromEnum r0 + fromEnum r1 * 13

  toEnum x = case compare d m of
    EQ -> Paired $ toEnum d
    LT -> Offsuited (toEnum m) (toEnum d)
    GT -> Suited (toEnum d) (toEnum m)
    where
      (d, m) = x `divMod` 13

instance Short Basis where
  short (Paired p) = short p <> short p
  short (Suited r0 r1) = short r0 <> short r1 <> "s"
  short (Offsuited r0 r1) = short r0 <> short r1 <> "o"

-- | convert from a Card pair to a Basis
--
-- The base mechanism of this transformation is to forget suit details.
--
-- >>> fromPair (Card Ace Hearts, Card Ace Spades)
-- Paired Ace
--
-- Unpaired cards are forced to high low order.
--
-- >>> fromPair (Card Two Hearts, Card Ace Spades)
-- Offsuited Ace Two
fromPair :: (Card, Card) -> Basis
fromPair (Card r s, Card r' s')
  | r == r' = Paired r
  | s == s' = Suited (max r r') (min r r')
  | otherwise = Offsuited (max r r') (min r r')

-- | Enumeration of the (Card,Card)'s that Basis represents.
--
-- >>> putStrLn $ Text.intercalate "." $ (\(x,y) -> short x <> short y) <$> toPairs (Paired Ace)
-- A♡A♣.A♡A♢.A♡A♠.A♣A♡.A♣A♢.A♣A♠.A♢A♡.A♢A♣.A♢A♠.A♠A♡.A♠A♣.A♠A♢
toPairs :: Basis -> [(Card, Card)]
toPairs (Paired r) = bimap (Card r) (Card r) <$> enum2 [Hearts .. Spades]
toPairs (Suited r0 r1) =
  ((\s -> (Card r0 s, Card r1 s)) <$> [Hearts .. Spades])
    <> ((\s -> (Card r1 s, Card r0 s)) <$> [Hearts .. Spades])
toPairs (Offsuited r0 r1) =
  ( bimap (Card r0) (Card r1)
      <$> enum2 [Hearts .. Spades]
  )
    <> ( bimap (Card r1) (Card r0)
           <$> enum2 [Hearts .. Spades]
       )

-- | a representative pair of cards for a B, choosing Hearts and Spades.
--
-- Always have a good think about this in the realm of raw card simulation.
toRepPair :: Basis -> (Card, Card)
toRepPair (Paired r) = (Card r Hearts, Card r Spades)
toRepPair (Suited r0 r1) = (Card r0 Hearts, Card r1 Hearts)
toRepPair (Offsuited r0 r1) = (Card r0 Hearts, Card r1 Spades)

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (p. fmap toEnum) . (\x y -> ishuffle [x, y]) <$> [0 .. (n -1)] <*> [0 .. (n -2)]
  where
    n = length xs
    p (x : y : _) = (xs List.!! x, xs List.!! y)
    p _ = error "list too short"

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
-- > shuffle 52 (take 52 rvs52) == ishuffle rvs52
ishuffle :: [Int] -> [Int]
ishuffle as = go as []
  where
    go [] dealt = reverse dealt
    go (x0 : xs) dealt = go xs (x1 : dealt)
      where
        x1 = foldl' (\acc d -> bool acc (acc + one) (d <= acc)) x0 (sort dealt)

-- | A Strat represents an array of a's indexed by the Basis.
--
-- Here is a chart of the chances of winning given a B, against another player with any 2.
--
-- ![bwin example](other/bwin.svg)
--
-- serialisation via Show, Read (in a pinch)
--
-- > (Strat $ fromList ((\((xs,_):_) -> xs) $ reads ((show $ toList $ array $ w2) :: String) :: [Double])) == w2
--
newtype Strat a = Strat
  { array :: Array '[169] a
  }
  deriving (Eq, Show, Foldable)

instance Functor Strat where
  fmap f (Strat a) = Strat (fmap f a)

instance Data.Distributive.Distributive Strat where
  distribute = distributeRep

instance Representable Strat where
  type Rep Strat = Basis

  tabulate f = Strat $ tabulate (f . toEnum . fromMaybe 0 . head)

  index (Strat a) = index a . (: []) . fromEnum

instance Short (Strat Text) where
  short s =
    Text.intercalate "\n"
    ((\x -> Text.intercalate " "
     -- 
       ((\y -> index s (toEnum $ 13 * (12 - x) + (12 - y))) <$>
        [0..12])) <$>
      [0..12])

instance Short (Strat Char) where
  short s =
    Text.intercalate "\n"
    ((\x -> Text.intercalate ""
       ((\y -> Text.singleton $ index s (toEnum $ 13 * (12 - x) + (12 - y))) <$>
        [0..12])) <$>
      [0..12])

instance Short (Strat Action) where
  short s = stratSym ("f","c","r","r") 10 s

-- | screen representation of a Strat
--
-- >>> pretty stratText
-- AA AKo AQo AJo ATo A9o A8o A7o A6o A5o A4o A3o A2o
-- AKs KK KQo KJo KTo K9o K8o K7o K6o K5o K4o K3o K2o
-- AQs KQs QQ QJo QTo Q9o Q8o Q7o Q6o Q5o Q4o Q3o Q2o
-- AJs KJs QJs JJ JTo J9o J8o J7o J6o J5o J4o J3o J2o
-- ATs KTs QTs JTs TT T9o T8o T7o T6o T5o T4o T3o T2o
-- A9s K9s Q9s J9s T9s 99 98o 97o 96o 95o 94o 93o 92o
-- A8s K8s Q8s J8s T8s 98s 88 87o 86o 85o 84o 83o 82o
-- A7s K7s Q7s J7s T7s 97s 87s 77 76o 75o 74o 73o 72o
-- A6s K6s Q6s J6s T6s 96s 86s 76s 66 65o 64o 63o 62o
-- A5s K5s Q5s J5s T5s 95s 85s 75s 65s 55 54o 53o 52o
-- A4s K4s Q4s J4s T4s 94s 84s 74s 64s 54s 44 43o 42o
-- A3s K3s Q3s J3s T3s 93s 83s 73s 63s 53s 43s 33 32o
-- A2s K2s Q2s J2s T2s 92s 82s 72s 62s 52s 42s 32s 22
stratText :: Strat Text
stratText = short <$> (tabulate id :: Strat Basis)

stratSym :: (Text, Text, Text, Text) -> Double -> Strat Action -> Text
stratSym (f,c,r,rr) b s =
    Text.intercalate "\n"
    ((\x -> Text.intercalate ""
       ((\y -> toSym $ index s (toEnum $ 13 * x + y)) <$>
        [0..12])) <$>
      [0..12])
    where
      toSym Fold = f
      toSym Call = c
      toSym (Raise x) = bool r rr (x > b)

-- | read a Strat from a text file.
--
-- > (Just o2') <- readStrat "other/o2.str" :: IO (Maybe (Strat Double))
-- > o2 == o2'
-- True
--
readStrat :: Read a => FilePath -> IO (Maybe (Strat a))
readStrat fp = fmap (Strat . fromList) . head . fmap fst . reads . unpack <$> readFile fp

-- | write a Strat to a text file.
--
-- > o2 = winOdds 2 1000
-- > writeStrat "other/o2.str" o2
--
writeStrat :: Show a => FilePath -> Strat a -> IO ()
writeStrat fp s = writeFile fp $ show $ toList $ array s

-- | Convert a Basis Map to a Strat.
fromMap :: Map.Map Basis a -> Strat (Maybe a)
fromMap m = tabulate (`Map.lookup` m)

-- | A typical poker table setup for texas holdem.
--
-- - each player gets 2 cards. There are typically 2 to 9 players.
--
-- - there are 5 hole cards
--
-- >>> tcards = deal cs
-- >>> pretty tcards
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠
--
data TableCards = TableCards
  { players :: Seq.Seq (Card, Card),
    hole :: Seq.Seq Card
  }
  deriving (Eq, Show, Generic)

instance NFData TableCards

instance Short TableCards where
  short (TableCards ps h) =
    Text.intercalate
      ","
      [ Text.intercalate " " $ (\(x, y) -> short x <> short y) <$> toList ps,
        mconcat $ short <$> toList h
      ]

-- | Deal table cards
deal :: [Card] -> TableCards
deal cs = do
  TableCards (fromList ((\x -> (cs List.!! (2 * x), cs List.!! (2 * x + 1))) <$> [0 .. n -1])) (fromList $ drop (n * 2) cs)
  where
    n = (length cs - 5) `div` 2

-- | For each seat, the betting can be open (can re-raise), closed (has called and cannot re-raise). A raise at the table re-opens the betting for all live seats.
--
-- SittingOut would be an extra sum type of you would need in live poker.
data Seat = BettingOpen | BettingClosed | Folded deriving (Eq, Show, Generic)

instance Short Seat where
  short BettingOpen = "o"
  short BettingClosed = "c"
  short Folded = "f"

instance NFData Seat

-- | Table state.
--
-- hero is poker jargon for a cursor into the next seat to act.
--
-- An alternative structure would be a Player type say, with card pair, Seat, stack, bet, but this seems artificial given likely computations that will be happening.
--
-- >>> t = dealTable defaultTableConfig cs
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
--
data Table = Table
  { cards :: TableCards,
    hero :: Maybe Int,
    seats :: Seq.Seq Seat,
    stacks :: Seq.Seq Double,
    bets :: Seq.Seq Double,
    pot :: Double,
    history :: Seq.Seq (Action, Int)
  }
  deriving (Eq, Show, Generic)

-- | number of seats at the table
--
-- >>> numSeats t
-- 2
numSeats :: Table -> Int
numSeats ts = length (ts ^. #seats)

instance NFData Table

instance Short Table where
  short (Table cs n s st bs p h) =
    Text.intercalate
      ","
      [ short cs,
        "hero: " <> show n,
        Text.intercalate " " $ short <$> toList s,
        Text.intercalate " " $ comma (Just 2) <$> toList st,
        Text.intercalate " " $ comma (Just 2) <$> toList bs,
        comma (Just 2) p,
        Text.intercalate ":" $ (\(a,p) -> short a <> show p) <$> toList h
      ]

-- | list of active player indexes
--
-- >>> liveSeats t
-- [0,1]
--
liveSeats :: Table -> [Int]
liveSeats ts =
  fst <$>
    filter
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
    fst <$>
    filter
    ((== BettingOpen) . snd)
    (nexts $ zip [0 ..] (toList $ ts ^. #seats))
    where
      nexts l = drop (h + 1) l <> take h l

-- | next seat open to bet
--
-- >>> nextHero t
-- Just 1
nextHero :: Table -> Maybe Int
nextHero ts = head (openSeats ts)

-- | The table is closed when no seat is open, or all but 1 seat has folded.
--
-- >>> closed t
-- False
closed :: Table -> Bool
closed ts =
  notElem BettingOpen (ts ^. #seats) ||
  length (filter (/= Folded) (toList $ ts ^. #seats)) <= 1

-- | Index of seat and hands in the pot
--
liveHands :: Table -> [(Int, [Card])]
liveHands ts = (\i -> hands (ts ^. #cards) List.!! i) <$> liveSeats ts

-- | Provide the player hands combined with the hole card.
hands :: TableCards -> [(Int, [Card])]
hands cs =
  zip [0 .. (length (cs ^. #players) - 1)] ((\(x, y) -> [x, y] <> toList (cs ^. #hole)) <$> toList (cs ^. #players))

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
--
dealTable :: TableConfig -> [Card] -> Table
dealTable cfg cs = Table (deal cs) (Just 0) (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0 Seq.Empty
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | standard blind and ante chip structure for n seats.
bbs :: Int -> Double -> Seq.Seq Double
bbs n ante = Seq.fromList $ reverse $ [1 + ante, 0.5 + ante] <> replicate (n -2) ante

-- | There are three primitive actions that seats must perform when they are the hero:
--
-- Fold. A seat can only fold if there are bets out above their current bet, and they have some chips. The Seat becomes Folded and the hand is discarded.
--
-- Call. A seat can bet the difference between the maximum bet and their current bet, or the rest of their stack if this is less. This is called a Call and is the minimum allowed bet action. A check is when zero is the minimum.
--
-- Raise x. A seat bets x above the minimum allowed bet, where x <= stack - minimum allowed.
--
data Action = Fold | Call | Raise Double deriving (Eq, Show, Generic)

instance NFData Action

instance Short Action where
  short Fold = "f"
  short Call = "c"
  short (Raise x) = fixed (Just 1) x <> "r"

-- | Always perform an action
always :: Action -> Strat Action
always a = tabulate (const a)

-- | Raise to the hero's stack size.
allin :: Table -> Strat Action
allin ts = tabulate (const (Raise x))
  where
    x = Seq.index (ts ^. #stacks) (fromMaybe 0 $ ts ^. #hero)

-- | A game progresses by players taking an action, which alters a table state.
--
-- >>> pretty t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
--
-- A 2 player table, where stacks start at 10 each, hero is seat 0, Big blind is seat 1. seat 1 posts the big blind, seat 0 posts the small blind. hero, as utg, is first action.
--
-- s0: Restricting the strategy action set to Fold, Call or Raise 10, seat 0 strategy (s0) branches into:
--
-- - s0: Fold
--
-- >>> pretty (actOn Fold t)
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 1,f o,9.5 9,0 1,0.5,f0
--
-- >>> closed (actOn Fold t)
-- True
--
-- - s0: Call
--
-- >>> pretty (actOn Call t)
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 1,c o,9 9,1 1,0,c0
--
-- s1: s1 is the strategy for seat 1, given betting history of [s0:Call]. They are open for betting (can actOn). They can Call or Raise 10
--
--     - s1: Call. At this point, we assume no further betting (this is equivalent to neither player having an advantage post-flop), and resolve the table.
--
-- >>> pretty $ actOn Call $ actOn Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Nothing,c c,9 9,1 1,0,c1:c0
--
-- Seat 0 wins a small pot without a showdown.
--
--     - s1: Raise 10
--
-- >>> pretty $ actOn (Raise 10) $ actOn Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9 0,1 10,0,10.0r1:c0
--
-- (s2) is the strategy for seat 0, given betting history of [s0:Call, s1:Raise 10]
--       - s2: Fold
--
-- >>> pretty $ actOn Fold $ actOn (Raise 10) $ actOn Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 1,f o,9 0,0 10,1,f0:10.0r1:c0
--
--       - s2: Call
-- >>> pretty $ actOn Call $ actOn (Raise 10) $ actOn Call t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 1,c o,0 0,10 10,0,c0:10.0r1:c0
--
-- Seat 0 wins a big pot with a pair of sevens
--
-- - s0: Raise 10
--
-- >>> pretty $ actOn (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 1,o o,0 9,10 1,0,10.0r0
--
-- (s3) is the strategy for seat 1, given betting history of [s0:Raise 10]
--
--     - s3:Fold
--
-- >>> pretty $ actOn Fold $ actOn (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o f,0 9,10 0,1,f1:10.0r0
--
--     - s3:Call
--
-- >>> pretty $ actOn Call $ actOn (Raise 10) t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o c,0 0,10 10,0,c1:10.0r0
actOn :: Action -> Table -> Table
actOn Fold ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
    & #bets %~ Seq.update p 0
    & #pot %~ (+ Seq.index (ts ^. #bets) p)
    & #seats
      %~ bool
        (Seq.update p BettingClosed)
        (Seq.update p Folded)
        (length (liveSeats ts) > 1)
    & #hero .~ nextHero ts
    & #history %~ ((Fold,p) Seq.:<|)
actOn Call ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
    & #bets %~ Seq.adjust' (+ bet) p
    & #stacks %~ Seq.adjust' (\x -> x - bet) p
    & #seats %~ Seq.update p BettingClosed
    & #hero .~ nextHero ts
    & #history %~ ((Call,p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min gap st
actOn (Raise r) ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
    & #bets %~ Seq.adjust' (+ bet) p
    & #stacks %~ Seq.adjust' (\x -> x - bet) p
    & bool
      (#seats %~ Seq.update p BettingClosed)
      ( (#seats %~ Seq.update p BettingOpen)
          . (#seats %~ fmap (\x -> bool x BettingOpen (x == BettingClosed)))
      )
      (st > gap)
    & (\x -> x & #hero .~ nextHero x)
    & #history %~ ((Raise r,p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min (gap + r) st

-- | Follow a betting pattern according to a strategy function, until betting is closed.
--
bet :: (Table -> Action) -> Table -> Table
bet f t0 = bool (bet f t1) t1 (closed t1)
  where
    t1 = actOn (f t0) t0

-- | Apply a strategy to a table, supplying the Action for the hero, if any.
apply :: Strat Action -> Table -> Maybe Action
apply s t = case t ^. #hero of
  Nothing  -> Nothing
  Just i -> Just $ index s (fromPair (Seq.index (t ^. #cards . #players) i))

-- | betting for a Table given a list of strategies, until betting is closed, or we run out of strategies.
--
-- This is almost:
--
-- > foldl (.) id (bet . apply <$> ss)
applys :: [Strat Action] -> Table -> Table
applys ss t = go 0 t
  where
    go n t = case apply (ss List.!! n) t of
      Nothing -> t
      Just a -> go (n+1) (actOn a t)

-- | 5 card standard poker rankings
--
-- >>> let junk = HighCard Ace King Ten Six Three
-- >>> (junk==) . toEnum . fromEnum $ junk
-- True
data HandRank
  = HighCard Rank Rank Rank Rank Rank
  | Pair Rank Rank Rank Rank
  | TwoPair Rank Rank Rank
  | ThreeOfAKind Rank Rank Rank
  | Straight Rank
  | Flush Rank Rank Rank Rank Rank
  | FullHouse Rank Rank
  | FourOfAKind Rank Rank
  | StraightFlush Rank
  deriving (Eq, Ord, Show, Generic)

instance NFData HandRank

instance Enum HandRank where
  fromEnum (HighCard r0 r1 r2 r3 r4) = sum $ zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r4, r3, r2, r1, r0]) [0 .. 4]
  fromEnum (Pair r0 r1 r2 r3) = (13 P.^ 5) + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r3, r2, r1, r0]) [0 .. 3 :: Int])
  fromEnum (TwoPair r0 r1 r2) = 13 P.^ 5 + 13 P.^ 4 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r2, r1, r0]) [0 .. 2])
  fromEnum (ThreeOfAKind r0 r1 r2) = 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r2, r1, r0]) [0 .. 2])
  fromEnum (Straight r0) = 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + fromEnum r0
  fromEnum (Flush r0 r1 r2 r3 r4) = 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r4, r3, r2, r1, r0]) [0 .. 4])
  fromEnum (FullHouse r0 r1) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 13 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r1, r0]) [0 .. 1])
  fromEnum (FourOfAKind r0 r1) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 13 P.^ 2 + 13 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r1, r0]) [0 .. 1])
  fromEnum (StraightFlush r0) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 2 * 13 P.^ 2 + 13 + fromEnum r0

  toEnum x
    | x < 13 P.^ 5 =
      (\(r0 : r1 : r2 : r3 : r4 : _) -> HighCard r0 r1 r2 r3 r4) $ fmap toEnum $ base13 x
    | x < 13 P.^ 5 + 13 P.^ 4 =
      (\(r0 : r1 : r2 : r3 : _) -> Pair r0 r1 r2 r3) $ fmap toEnum $ base13 (x - 13 P.^ 5)
    | x < 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 =
      (\(r0 : r1 : r2 : _) -> TwoPair r0 r1 r2) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 =
      (\(r0 : r1 : r2 : _) -> ThreeOfAKind r0 r1 r2) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 =
      Straight (toEnum $ x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3))
    | x < 2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 =
      (\(r0 : r1 : r2 : r3 : r4 : _) -> Flush r0 r1 r2 r3 r4) $ fmap toEnum $ base13 x
    | x < 2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 P.^ 2 + 13 =
      (\(r0 : r1 : _) -> FullHouse r0 r1) $ fmap toEnum $ base13 (x - (2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13 =
      (\(r0 : r1 : _) -> FourOfAKind r0 r1) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 2 * 13 =
      StraightFlush (toEnum $ x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13))
    | otherwise = StraightFlush Ace

-- | converts from an integral to base 13, as a list.
base13 :: (Eq a, Num a, Integral a) => a -> [a]
base13 x = go x []
  where
    go 0 l = l
    go acc l = let (d, m) = acc `divMod` 13 in go d (m : l)

instance Short HandRank where
  short (HighCard r0 r1 r2 r3 r4) =
    " H:"
      <> short r0
      <> short r1
      <> short r2
      <> short r3
      <> short r4
  short (Pair r0 r1 r2 r3) =
    " P:"
      <> short r0
      <> short r1
      <> short r2
      <> short r3
  short (TwoPair r0 r1 r2) =
    "2P:"
      <> short r0
      <> short r1
      <> short r2
  short (ThreeOfAKind r0 r1 r2) =
    " 3:"
      <> short r0
      <> short r1
      <> short r2
  short (FourOfAKind r0 r1) =
    " 4:"
      <> short r0
      <> short r1
  short (FullHouse r0 r1) =
    "32:"
      <> short r0
      <> short r1
  short (Straight r0) =
    " S:"
      <> short r0
  short (Flush r0 r1 r2 r3 r4) =
    " F:"
      <> short r0
      <> short r1
      <> short r2
      <> short r3
      <> short r4
  short (StraightFlush r0) =
    "SF:"
      <> short r0

-- | compute a HandRank from a list of Cards.
--
-- Should work for 5 and 7 hand variants.
--
-- >>> let cs = [Card {rank = Ace, suit = Hearts},Card {rank = Seven, suit = Spades},Card {rank = Ten, suit = Hearts},Card {rank = Five, suit = Spades},Card {rank = Six, suit = Clubs},Card {rank = Seven, suit = Hearts},Card {rank = Six, suit = Spades}]
-- >>> pretty cs
-- A♡7♠T♡5♠6♣7♡6♠
--
-- >>> handRank cs
-- Just (TwoPair Seven Six Ace)
--
handRank :: [Card] -> Maybe HandRank
handRank cs =
  flush cs'
    <|> Straight <$> straight (toList $ ranks cs')
    <|> kind (rank <$> cs')
  where
    cs' = sortOn Down cs

-- | maybe convert cards to a Straight (consecutive card runs)
--
-- Special rules for an Ace, which can be counted as high or low.
--
-- >>> straight [Ace, King, Queen, Jack, Ten, Nine, Eight]
-- Just Ace
--
-- >>> straight [Ace, King, Queen, Jack, Ten, Eight, Seven]
-- Just Ace
--
-- >>> straight [Ace, King, Queen, Five, Four, Three, Two]
-- Just Five
--
-- >>> straight [Ace, King, Queen, Six, Four, Three, Two]
-- Nothing
straight :: [Rank] -> Maybe Rank
straight [] = Nothing
straight rs@(Ace:rs') = maybe (bool Nothing (Just Five) (run4 rs' == Just Five)) Just (run5 rs)
straight rs = run5 rs

run5 :: [Rank] -> Maybe Rank
run5 rs = head $ fst <$> filter ((>= 5) . snd) (runs rs)

run4 :: [Rank] -> Maybe Rank
run4 rs = head $ fst <$> filter ((>= 4) . snd) (runs rs)

runs :: [Rank] -> [(Rank, Int)]
runs rs = done (foldl' step (Nothing, []) rs)
  where
    step (Nothing, _) r = (Just (r,r), [])
    step (Just (r1,r0), xs) r =
       bool
       -- if gapped then reset, remember old gap
       (Just (r,r), (r0, fromEnum r0 - fromEnum r1 + 1):xs)
       -- if one less then do nothing
       (Just (r, r0), xs)
       (fromEnum r + one == fromEnum r1)
    done (Nothing, xs) = xs
    done (Just (r1,r0), xs) = (r0, fromEnum r0 - fromEnum r1 + 1):xs

-- | maybe convert cards to a Flush or StraightFlush
--
-- >>> flush [Card Ace Hearts, Card Seven Clubs, Card Seven Spades, Card Five Hearts, Card Four Hearts, Card Three Hearts, Card Two Hearts]
-- Just (StraightFlush Five)
flush :: [Card] -> Maybe HandRank
flush cs =
  case filter ((>= 5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (straight rs)
    _ -> Nothing

suitRanks :: [Card] -> [(Suit, [Rank])]
suitRanks cs =
  Map.toList $
    Map.fromListWith (flip (<>)) $
      fmap (\(Card r s) -> (s, [r])) cs

rankCount :: [Rank] -> [(Rank, Int)]
rankCount rs =
  sortOn (Down . swap) $
    Map.toList $
      Map.fromDescListWith (+) $
        fmap (,1) rs

-- | When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> kind [Ace, Ace, Ace, Ace, Two]
-- Just (FourOfAKind Ace Two)
--
-- >>> kind [Ace, Ace, Ace, Two, Two]
-- Just (FullHouse Ace Two)
--
-- >>> kind [Ace, Ace, Ace, Five, Two]
-- Just (ThreeOfAKind Ace Five Two)
--
-- >>> kind [Ace, Ace, Five, Five, Two]
-- Just (TwoPair Ace Five Two)
--
-- >>> kind [Ace, Ace, Six, Five, Two]
-- Just (Pair Ace Six Five Two)
--
-- >>> kind [Ace, King, Six, Five, Two]
-- Just (HighCard Ace King Six Five Two)
--
kind :: [Rank] -> Maybe HandRank
kind cs = case rankCount cs of
  ((r0, 4):(r1, 1):_) -> Just $ FourOfAKind r0 r1
  ((r0, 3):(r1, 2):_) -> Just $ FullHouse r0 r1
  ((r0, 3):(r1, 1):(r2, 1):_) -> Just $ ThreeOfAKind r0 r1 r2
  ((r0, 2):(r1, 2):(r2, 1):_) -> Just $ TwoPair r0 r1 r2
  ((r0, 2):(r1, 1):(r2, 1):(r3, 1):_) -> Just $ Pair r0 r1 r2 r3
  ((r0, 1):(r1, 1):(r2, 1):(r3, 1):(r4, 1):_) -> Just $ HighCard r0 r1 r2 r3 r4
  _ -> Nothing

-- $performance
--
-- > Perf.tick handRank cs
-- (328006,Just (TwoPair Four Queen Five))
--

-- | Ship the pot to the winning hands
--
-- >>> pretty $ showdown t
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,11 9,0 0,0,
showdown :: Table -> Table
showdown ts =
  ts
    & #stacks %~ (\s -> foldr ($) s (Seq.adjust' (+ pot' / fromIntegral (length winners)) <$> winners))
    & #bets .~ fromList (replicate (numSeats ts) 0)
    & #pot .~ 0
  where
    pot' = sum (ts ^. #bets) + (ts ^. #pot)
    winners = bestLiveHand ts

-- | index of the winning hands
--
-- >>> bestLiveHand t
-- [0]
bestLiveHand :: Table -> [Int]
bestLiveHand ts =
  mconcat $ maybeToList $
    fmap (fmap fst) $
      head $
        List.groupBy
          (\x y -> (snd x :: Maybe HandRank) == snd y)
          (sortOn (Down . snd) (second handRank <$> liveHands ts))
