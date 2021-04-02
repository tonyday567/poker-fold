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
module Poker where

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
data B = Suited Rank Rank | Offsuited Rank Rank | Paired Rank deriving (Eq, Show, Ord)

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
btoc (Offsuited r0 r1) = (Card r0 Hearts, Card r1 Spades)
btoc (Suited r0 r1) = (Card r0 Hearts, Card r1 Hearts)

instance Enum B where
  fromEnum (Paired p) = fromEnum p * 13 + fromEnum p
  fromEnum (Suited r0 r1) = fromEnum r0 + fromEnum r1 * 13
  fromEnum (Offsuited r0 r1) = fromEnum r0 * 13 + fromEnum r1

  toEnum x = case compare d m of
    EQ -> Paired $ toEnum d
    LT -> Suited (toEnum m) (toEnum d)
    GT -> Offsuited (toEnum d) (toEnum m)
    where
      (d,m) = x `divMod` 13

instance Short B where
  short (Paired p) = short p <> short p
  short (Suited r0 r1) = short r0 <> short r1 <> "s"
  short (Offsuited r0 r1) = short r0 <> short r1 <> "o"

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

-- | enumeration of all the B's
--
-- This chart compares the chances of winning in a 2 player game versus a 9 player game, given hero is holding a B.
--
--
bs :: Strat B
bs = Strat $ fromList $ toEnum <$> [0..168]

-- | count the Bs in a full enumeration of dealing two cards.
--
-- uses enumeration rather than is random
--
-- >>> take 5 $ reverse $ toList $ array countBs
-- [Paired Ace,Paired Ace,Paired Ace,Suited Ace King,Offsuited Ace King]
countBs :: Strat Int
countBs = tabulate (\k -> fromMaybe zero $ Map.lookup k (Map.fromListWith (+) ((,1) . c2b <$> enum2 deck)))

dealB :: (RandomGen g) => Int -> B -> State g [Card]
dealB p b = dealNWith (5+2*p) (deck List.\\ (\(x,y)->[x,y]) (btoc b))

-- >>> let a = evalState (tableStateB 1 (Paired Ace)) (mkStdGen 42)
tableStateB :: (RandomGen g) => Int -> B -> State g TableState
tableStateB p b = do
  cs <- dealB p b
  pure $ TableState ([btoc b] <> ((\x -> (cs List.!! (2*x), cs List.!! (2*x+1))) <$> [0..p-1])) (drop (p*2) cs)

-- | The minimum 2-player game consists of the hero (p0) headsup versus the enemy (p1).
--
-- The hero can Fold, and we can consider this to have value zero, and 1.5b for the enemy. (0b)
-- The hero can Call, by convention, this costs half a blind.
-- The hero can raise AllIn x.
-- On a Call, the enemy can Check. At this point, we assume no further betting, and resolve. (+1.5,-0.5)
-- On a Call, the enemy can AllIn x-0.5. hero can Fold (-0.5b). hero can Call (x+1.5b, -x)
-- On an AllIn x, enemy can Fold (+1.5b) or Call (x+1.5b,-x)

data Action = Fold | Call | AllIn Double

-- | uniform random variate of an Int, typically an index into a structure.
rvi :: (RandomGen g) => Int -> State g Int
rvi n = do
  g <- get
  let (x,g') = uniformR (0, n - 1) g
  put g'
  pure x

-- | finite population n samples without replacement
--
rvis :: (RandomGen g) => Int -> Int -> State g [Int]
rvis n k = sequence (rvi . (n -) <$> [0..(k-1)])

-- | a valid series of random index values to shuffle a population of 52 enums
--
-- >>> rvs52
-- [48,23,31,15,16,18,17,23,11,31,5,14,30,28,27,2,9,11,27,24,17,0,10,2,2,11,8,2,18,8,11,16,6,14,3,1,6,0,2,11,1,6,3,7,4,1,5,4,2,1,0,0]
rvs52 :: [Int]
rvs52 = flip evalState (mkStdGen 42) $ rvis 52 52

-- | vector perfect shuffle
--
-- >>> shuffle 52 rvs52
-- ([48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42],[])
shuffle :: Int -> [Int] -> (V.Vector Int, V.Vector Int)
shuffle n =
  foldl'
  (\(dealt, rem) i ->
     let (x,rem') = cutV rem i in (V.snoc dealt x, rem'))
  (V.empty, V.enumFromN 0 n)

-- | cut a vector at n, returning the n'th element, and the truncated vector
cutV :: V.Vector a -> Int -> (a, V.Vector a)
cutV v x =
  (v V.! x,
   V.unsafeSlice 0 x v <> V.unsafeSlice (x+1) (n - x - 1) v)
  where
    n = V.length v


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

-- | deal n cards from a fresh, shuffled, standard pack.
--
-- >>>  putStrLn $ Text.intercalate "\n" $ fmap short <$> flip evalState (mkStdGen 44) $ replicateM 5 (dealN 5)
-- A♣3♠K♠7♡9♠
-- 9♠7♡2♣Q♢J♣
-- K♢4♣9♢K♠7♠
-- 7♣7♠J♡8♡J♢
-- 5♠Q♣A♣Q♡T♠
--
dealN :: (RandomGen g) => Int -> State g [Card]
dealN n = fmap toEnum . ishuffle <$> rvis 52 n

dealN' :: (RandomGen g) => Int -> State g [Card]
dealN' n = fmap toEnum . V.toList . fst . shuffle 52 <$> rvis 52 n

dealNWith :: (RandomGen g) => Int -> [Card] -> State g [Card]
dealNWith n cs = fmap (cs List.!!) . ishuffle <$> rvis (length cs) n

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (\(x:y:_) -> (xs List.!! x, xs List.!! y)) $ fmap (fmap toEnum) . (\x y -> ishuffle [x,y]) <$> [0..(n-1)] <*> [0..(n-2)]
  where
    n = length xs

newtype Hand = Hand { cards :: [Card] } deriving (Eq, Show, Generic)

instance Short Hand where
  short (Hand cs) =
    Text.intercalate "" (fmap short cs)

instance NFData Hand

hand :: [Card] -> Hand
hand cs = Hand $ sortOn Down cs

ranks :: Hand -> Set.Set Rank
ranks (Hand cs) = Set.fromDescList $ rank <$> cs

suits :: Hand -> Set.Set Suit
suits (Hand cs) = Set.fromList $ suit <$> cs

-- | 5 card standard poker rankings
--
-- >>> toEnum $ fromEnum (HighCard Ace King Ten Six Three) :: HandRank
-- 
data HandRank = HighCard Rank Rank Rank Rank Rank
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
  fromEnum (HighCard r0 r1 r2 r3 r4) = sum $ zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r4,r3,r2,r1,r0]) [0..4]
  fromEnum (Pair r0 r1 r2 r3) = (13 P.^ 5) + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r3,r2,r1,r0]) [0..3::Int])
  fromEnum (TwoPair r0 r1 r2) = 13 P.^ 5 + 13 P.^ 4 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r2,r1,r0]) [0..2])
  fromEnum (ThreeOfAKind r0 r1 r2) = 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + sum (zipWith (\r i -> r * 13  P.^  i) (fromEnum <$> [r2,r1,r0]) [0..2])
  fromEnum (Straight r0) = 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + fromEnum r0
  fromEnum (Flush r0 r1 r2 r3 r4) = 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 + sum (zipWith (\r i -> r * 13 P.^ i) (fromEnum <$> [r4,r3,r2,r1,r0]) [0..4])
  fromEnum (FullHouse r0 r1) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 13 + sum (zipWith (\r i -> r * 13  P.^  i) (fromEnum <$> [r1,r0]) [0..1])
  fromEnum (FourOfAKind r0 r1) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 13 P.^ 2 + 13 + sum (zipWith (\r i -> r * 13  P.^  i) (fromEnum <$> [r1,r0]) [0..1])
  fromEnum (StraightFlush r0) = 2 * 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 + 2 * 13 P.^ 2 + 13 + fromEnum r0

  toEnum x
    | x < 13 P.^ 5 =
      (\(r0:r1:r2:r3:r4:_) -> HighCard r0 r1 r2 r3 r4) $ fmap toEnum $ base13 x
    | x < 13 P.^ 5 + 13 P.^ 4 =
      (\(r0:r1:r2:r3:_) -> Pair r0 r1 r2 r3) $ fmap toEnum $ base13 (x - 13 P.^ 5)
    | x < 13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3 =
      (\(r0:r1:r2:_) -> TwoPair r0 r1 r2) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 =
      (\(r0:r1:r2:_) -> ThreeOfAKind r0 r1 r2) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4 + 13 P.^ 3))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 =
      Straight (toEnum $ x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3))
    | x < 2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 =
      (\(r0:r1:r2:r3:r4:_) -> Flush r0 r1 r2 r3 r4) $ fmap toEnum $ base13 x
    | x < 2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13 P.^ 2 + 13 =
      (\(r0:r1:_) -> FullHouse r0 r1) $ fmap toEnum $ base13 (x - (2 * 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 13))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13 =
      (\(r0:r1:_) -> FourOfAKind r0 r1) $ fmap toEnum $ base13 (x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13))
    | x < 13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 2 * 13 =
      StraightFlush (toEnum $ x - (13 P.^ 5 + 13 P.^ 4 + 2 * 13 P.^ 3 + 2 * 13 P.^ 2 + 13))
    | otherwise = StraightFlush Ace

base13 :: (Eq a, Num a, Integral a) => a -> [a]
base13 x = go x []
  where
    go 0 l = l
    go acc l = let (d,m) = acc `divMod` 13 in go d (m:l)


-- | compute the hand ranking
--
-- >>> let h = hand $ flip evalState (mkStdGen 55) $ dealN 7
-- >>> Perf.tick handRank h
-- (328006,Just (TwoPair Four Queen Five))
--
-- >>> putStrLn =<< (\(c,t) -> comma (Just 2) (fromIntegral (P.toInteger c)/2.6e3 :: Double) <> "\181: " <> (show t :: Text)) <$> Perf.tick (handRank . fst . winner) (ts List.!! 0)
-- 929µ: Just (TwoPair Nine Ace Three)
--
-- >>> putStrLn =<< (\(c,t) -> comma (Just 2) (fromIntegral (P.toInteger c) :: Double) <> " " <> (show t :: Text)) <$> Perf.tick (handRank . fst . winner) (ts List.!! 0)
-- 2.40e6 Just (TwoPair Nine Ace Three)
handRank :: Hand -> Maybe HandRank
handRank h =
  flush h <|>
  straight h <|>
  kind h

straight :: Hand -> Maybe HandRank
straight h = Straight <$> str8 (toList $ ranks h)

rangeCount :: (Enum a) => Mealy a [(a,a)]
rangeCount = M (\a -> ((a,a),[])) (\((rmin,rmax), rs) a -> bool ((a,a),(rmin,rmax):rs) ((rmin,a),rs) (fromEnum a == fromEnum rmax + one)) (\x -> fst x:snd x)

str8 :: Enum a => [a] -> Maybe a
str8 xs = case runs of
  [] -> Nothing
  ((_,r):_) -> Just r
  where
    runs = filter (\(x,y) -> fromEnum y - fromEnum x >= 4) (Data.Mealy.fold rangeCount xs)

flush :: Hand -> Maybe HandRank
flush h =
  case filter ((>=5) . length . snd) (suitRanks h) of
    [] -> Nothing
    ((_,rs@(r0:r1:r2:r3:r4:_)):_) ->
      Just $
      maybe
      (Flush r0 r1 r2 r3 r4)
      StraightFlush
      (str8 rs)
    _ -> Nothing

suitRanks :: Hand -> [(Suit, [Rank])]
suitRanks (Hand cs) =
  Map.toList $
  Map.fromDescListWith (<>) $
  fmap (\(Card r s) -> (s,[r])) cs

rankCount :: Hand -> [(Rank,Int)]
rankCount (Hand cs) =
  sortOn (Down . swap) $
  Map.toList $
  Map.fromDescListWith (+) $
  fmap (\(Card r _) -> (r,1)) cs

kind :: Hand -> Maybe HandRank
kind h = case rankCount h of
  [] -> Nothing
  ((r0,n0):rs) -> case n0 of
    2 -> case rs of
      [] -> Nothing
      ((r1,n1):rs') ->
        case rs' of
          [] -> Nothing
          ((r2,_):rs'') ->
            bool
            (case rs'' of
               [] -> Nothing
               ((r3,_):_) -> Just $ Pair r0 r1 r2 r3)
            (Just $ TwoPair r0 r1 r2)
            (n1==2)
    1 -> case rs of
      [] -> Nothing
      ((r1,_):rs') ->
        case rs' of
          [] -> Nothing
          ((r2,_):rs'') ->
            case rs'' of
              [] -> Nothing
              ((r3,_):rs''') ->
                case rs''' of
                  [] -> Nothing
                  ((r4,_):_) -> Just $ HighCard r0 r1 r2 r3 r4
    3 -> case rs of
      [] -> Nothing
      ((r1,n1):rs') -> case rs' of
        [] -> Nothing
        ((r2,_):_) -> Just $
          bool
          (ThreeOfAKind r0 r1 r2)
          (FullHouse r0 r1)
          (n1>1)
    4 -> case rs of
      [] -> Nothing
      ((r1,_):_) -> Just $ FourOfAKind r0 r1
    _ -> Nothing

instance Short HandRank where
  short (HighCard r0 r1 r2 r3 r4) = " H:" <>
    short r0 <> short r1 <> short r2 <> short r3 <> short r4
  short (Pair r0 r1 r2 r3) = " P:" <>
    short r0 <> short r1 <> short r2 <> short r3
  short (TwoPair r0 r1 r2) = "2P:" <>
    short r0 <> short r1 <> short r2
  short (ThreeOfAKind r0 r1 r2) = " 3:" <>
    short r0 <> short r1 <> short r2
  short (FourOfAKind r0 r1) = " 4:" <>
    short r0 <> short r1
  short (FullHouse r0 r1) = "32:" <>
    short r0 <> short r1
  short (Straight r0) = " S:" <>
    short r0
  short (Flush r0 r1 r2 r3 r4) = " F:" <>
    short r0 <> short r1 <> short r2 <> short r3 <> short r4
  short (StraightFlush r0) = "SF:" <>
    short r0

bestHand :: [Hand] -> Int
bestHand hs =
  fromMaybe 0 $
  head $
  fmap snd $
  sortOn (Down . handRank . fst) (zip hs [0..(length hs - 1)])

data TableState = TableState
  { players :: [(Card, Card)],
    hole :: [Card]
  } deriving (Eq, Show, Generic)

instance NFData TableState

instance Short TableState where
  short (TableState ps h) =
    Text.intercalate ","
    (short <$> toList ps) <> ":" <>
    Text.intercalate " "
     [ short (take 3 h),
       short (take 1 $ drop 3 h),
       short (take 1 $ drop 4 h)
     ]

-- | deal a random table
--
-- >>> fst <$> Perf.tick (\x -> evalState (replicateM x (dealTable 9)) (mkStdGen 42)) 100
-- 36849002
--
-- >>> fst <$> Perf.tick (\x -> evalState (replicateM x (dealTable 9)) (mkStdGen 42)) 1000
-- 356727876
--
-- let t = evalState (dealTable 2) (mkStdGen 42)
-- >>> pretty t
-- 7♡6♠,9♡4♠:A♡7♠T♡ 5♠ 6♣
--
dealTable :: (RandomGen g) => Int -> State g TableState
dealTable n = do
  cs <- dealN' (5 + 2 * n)
  pure $
    TableState
      ((\x -> (\(x:y:_) -> (x,y)) $ take 2 $ drop (5+x*2) cs) <$> [0..(n-1)])
      (take 5 cs)

-- | Provide the best hand for each player
-- >>> pretties $ resolve t
-- A♡7♠7♡6♣6♠
-- A♡T♡9♡7♠6♣
--
hands  :: TableState -> [Hand]
hands (TableState ps h) =
  (\(x,y) -> hand ([x,y] <> h)) <$> ps

-- | determine the winner for a table
--
-- >>> let t = evalState (dealTable 9) (mkStdGen 42)
-- >>> pretty t
-- pretty t
-- 7♡6♠,9♡4♠,J♠3♣,6♢Q♣,J♢J♣,2♢5♡,6♡K♡,Q♡9♣,2♡7♣:A♡7♠T♡ 5♠ 6♣
-- >>> winner t
-- putStrLn $ (\(x,y) -> y <> ": " <> x) $ bimap short show $ winner t
-- 0: A♡7♠7♡6♣6♠
--
-- first player wins with two pair.
--
winner :: TableState -> Int
winner ts = bestHand (hands ts)


winB :: B -> Int -> Int -> Double
winB b p n = (/fromIntegral n) $ sum $ bool 0 (1::Double) . (0==) . winner <$> evalState (replicateM n (tableStateB p b)) (mkStdGen 42)

ordB :: Int -> Int -> [(B,Double)]
ordB p n = sortOn (Down . snd) $ zip (toEnum <$> [0..168]) $ toList $ array $ (\x -> winB x p n) <$> bs

compare29 :: B -> Point Double
compare29 x = Point (winB x 1 1000) (winB x 8 1000)

compare29s :: [Point Double]
compare29s = compare29 . toEnum <$> [0..168]

