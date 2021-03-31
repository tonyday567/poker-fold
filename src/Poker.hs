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
-- import Data.Functor.Contravariant
-- import Data.Functor.Contravariant
import Chart hiding (shape)
import Lens.Micro
import Perf hiding (zero)
import qualified Data.Vector as V
import GHC.TypeLits
import qualified NumHask.Array.Shape as Shape
import qualified Control.Scanl as Scan
import Data.Mealy
import qualified Data.List as List

class Short a where
  short :: a -> Text

  pretty :: a -> IO ()
  pretty = putStrLn . short

  pretties :: (Foldable f) => f a -> IO ()
  pretties xs = putStrLn $ Text.intercalate "\n" $ short <$> toList xs

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

data Suit = Hearts | Clubs | Diamonds| Spades deriving (Eq, Show, Ord, Enum, Generic)

instance NFData Suit

-- | see https://decodeunicode.org/en/u+1F0A2
instance Short Suit where
  short Hearts = "\9825"
  short Clubs = "\9827"
  short Diamonds = "\9826"
  short Spades = "\9824"

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

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (\(x:y:_) -> (xs List.!! x, xs List.!! y)) $ fmap (fmap toEnum) . (\x y -> ishuffle [x,y]) <$> [0..(n-1)] <*> [0..(n-2)]
  where
    n = length xs

newtype Hand = Hand { cards :: Seq.Seq Card } deriving (Eq, Show, Generic)

instance Short Hand where
  short (Hand cs) =
    Text.intercalate "" (toList $ fmap short cs)

instance NFData Hand

hand :: Seq.Seq Card -> Hand
hand cs = Hand $ Seq.sortOn Down cs

ranks :: Hand -> Set.Set Rank
ranks (Hand cs) = Set.fromDescList $ toList $ rank <$> cs

suits :: Hand -> Set.Set Suit
suits (Hand cs) = Set.fromList $ toList $ suit <$> cs

data HandRank = HighCard (Seq.Seq Rank)
              | Pair  (Seq.Seq Rank)
              | TwoPair (Seq.Seq Rank)
              | ThreeOfAKind (Seq.Seq Rank)
              | Straight (Seq.Seq Rank)
              | Flush (Seq.Seq Rank)
              | FullHouse (Seq.Seq Rank)
              | FourOfAKind (Seq.Seq Rank)
              | StraightFlush (Seq.Seq Rank)
              deriving (Eq, Ord, Show)

instance Short HandRank where
  short (HighCard rs) = " 1:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Pair rs) = " 2:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (TwoPair rs) = "22:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (ThreeOfAKind rs) = " 3:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (FourOfAKind rs) = " 4:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (FullHouse rs) = "32:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Straight rs) = " S:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Flush rs) = " F:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (StraightFlush rs) = "SF:" <>
    Text.intercalate "" (toList $ fmap short rs)

-- | compute the hand rank
--
-- >>> pretties $ handRank <$> hs
--  1:AT765
--  2:7J94
--  2:JQ73
--  1:KQ752
--  2:2973
--  2:8K73
--  2:KT74
--  2:AK43
--  1:AKT84
-- 22:AT7
handRank :: Hand -> HandRank
handRank h = fromMaybe (kind h)
  (sflush h <|>
   flush h <|>
   straight h)

-- | provide the ranks if the cards are an exact flush.
--
-- >>> flush (Seq.fromList [Card Ace Hearts, Card King Hearts, Card Queen Hearts])
-- Just (Flush (fromList [Queen,King,Ace]))
--
-- >>> flush (Seq.fromList [Card Ace Hearts, Card King Hearts, Card Queen Hearts, Card Jack Spades])
-- Nothing
flush :: Hand -> Maybe HandRank
flush h =
  bool
  Nothing
  (Just $ Flush $ rankFlush h)
  (isFlush h)

isFlush :: Hand -> Bool
isFlush h = 1 == Set.size (suits h)

rankFlush :: Hand -> Seq.Seq Rank
rankFlush h = fromList $ Set.toDescList $ ranks h

straight :: Hand -> Maybe HandRank
straight h =
  bool
  Nothing
  (Just $ Straight (Seq.singleton (rankStraight h)))
  (isStraight h)

isStraight :: Hand -> Bool
isStraight h =
  4 == (fromEnum (Set.findMax (suits h)) - fromEnum (Set.findMin (suits h)))

rankStraight :: Hand -> Rank
rankStraight h = Set.findMax (ranks h)

isSFlush :: Hand -> Bool
isSFlush h = isFlush h && isStraight h

sflush :: Hand -> Maybe HandRank
sflush h =
  bool
  Nothing
  (Just $ StraightFlush (Seq.singleton (rankStraight h)))
  (isSFlush h)

kind :: Hand -> HandRank
kind h = let rx@((r, x) Seq.:<| xs) = rankCount h in
  case x of
    4 -> FourOfAKind (Seq.singleton r)
    3 -> case xs of
          Seq.Empty -> ThreeOfAKind (Seq.singleton r)
          ((r', x') Seq.:<| _) ->
            bool
            (ThreeOfAKind (r Seq.:<| (fst <$> xs)))
            (FullHouse (r Seq.:<| r' Seq.:<| Seq.Empty)) (x'==2)
    2 -> case xs of
          Seq.Empty -> Pair (Seq.singleton r)
          ((r', x') Seq.:<| xs') ->
            bool
            (Pair (r Seq.:<| fmap fst xs))
            (TwoPair $ r Seq.:<| r' Seq.:<| fmap fst xs')
            (x' == 2)
    1 -> HighCard (fmap fst rx)
    _ -> HighCard (fmap fst rx)

rankCount :: Hand -> Seq.Seq (Rank, Int)
rankCount (Hand cs) =
  Seq.sortOn (\(r,x) -> Down (x,r)) $
  fromList $
  Map.toList $
  Map.fromDescListWith (+) $
  toList $
  fmap ((,1) . rank) cs

rip75 :: [a] -> [[a]]
rip75 s = mconcat $ (`ripHole2` s) <$> [0 .. (length s - 1)]
  where
    ripHole2 x s = let (s0, s1) = cut x s in
      (s0 <>) . (\y -> uncurry (<>) $ cut y s1) <$> [0 .. (length s1 - 1)]
    cut x s = (take x s, drop (x+1) s)

bestHand :: [Hand] -> (Hand, Int)
bestHand hs =
  fromMaybe (Hand (Seq.fromList []), 0) $
  head $
  sortOn (Down . handRank . fst) (zip hs [0..(length hs - 1)])

data TableState = TableState
  { players :: [[Card]],
    hole :: [Card]
  } deriving (Eq, Show, Generic)

instance Short TableState where
  short (TableState ps h) =
    Text.intercalate ","
    (short <$> toList ps) <> ":" <>
    Text.intercalate " "
     [ short (take 3 h),
       short (take 1 $ drop 3 h),
       short (take 1 $ drop 4 h)
     ]

-- | deal a table from state
--
-- >>> fst <$> Perf.tick (\x -> evalState (fmap (fst . winner) <$> replicateM x (dealTable 9)) (defaultPackState 42)) 100
-- 2259275382
--
-- So this is 100 events x 21 (rip75 combos) x 9 (num players) = 18900 handRank evaluations.
-- 2259275382 / 18900 = 119k cycles per evaluation.
--
-- 611672144 / 10000 / 5 * 23 (num cards) = 281k cycles per table draw * 100 = 28m out of 2259m total cycles.
--
--
dealTable :: (RandomGen g) => Int -> State g TableState
dealTable n = do
  cs <- dealN (5 + 2 * n)
  pure $
    TableState
      ((\x -> take 2 $ drop (x*2) cs) <$> [0..(n-1)])
      (take 5 cs)

-- | Provide the best hand for each player
-- >>> ts0 = evalState (dealTable 1) defaultPackState
-- >>> resolve5 ts0
--
resolve5 :: TableState -> [Hand]
resolve5 (TableState ps h) = fmap fst $
  (\p -> bestHand $ hand . Seq.fromList <$> rip75 (p <> h)) <$> ps

-- | determine the winner for a table
--
-- >>> ts = evalState (replicateM 1000 (dealTable 9)) defaultPackState
-- >>> sum $ snd . winner <$> ts
-- 4052
winner :: TableState -> (Hand, Int)
winner ts = bestHand (resolve5 ts)

data B = Suited Rank Rank | Offsuited Rank Rank | Paired Rank deriving (Eq, Show, Ord)

instance Enum B where
  -- dense packing
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

bs :: Strat B
bs = Strat $ fromList $ toEnum <$> [0..168]

asMatrix :: Strat a -> Array '[13,13] a
asMatrix (Strat a) = reshape a

c2b :: Card -> Card -> B
c2b (Card r s) (Card r' s')
  | r==r' = Paired r
  | s==s' = Suited (max r r') (min r r')
  | otherwise = Offsuited (max r r') (min r r')

mapb :: Map B Int
mapb = Map.fromListWith (+) ((,1) . uncurry c2b <$> enum2 deck)

-- | count the Bs in a full enumeration of dealing two cards.
--
-- uses enumeration rather than is random
--
-- >>> take 5 $ reverse $ toList $ array countBs
-- [Paired Ace,Paired Ace,Paired Ace,Suited Ace King,Offsuited Ace King]
countBs :: Strat Int
countBs = tabulate (\k -> fromMaybe zero $ Map.lookup k mapb)

stratText :: ChartSvg
stratText =
  mempty &
  #hudOptions .~ mempty &
  #chartList .~
  [ Chart (TextA
           ( defaultTextStyle &
             #size .~ 0.03
           )
           (toList $ short <$> array bs))
    (PointXY . (\(Point x y) -> Point (-x) y) <$> gs)]
  where
    gs = grid MidPos (one :: Rect Double) (Point 13 13) :: [Point Double]

-- | Enumeration of (Card,Card) that B represents.
--
-- >>> countBs == (length . bToCards <$> bs)
-- True
bToCards :: B -> [(Card, Card)]
bToCards (Paired r) = bimap (Card r) (Card r) <$> enum2 [Hearts .. Spades]
bToCards (Suited r0 r1) =
  ((\s -> (Card r0 s, Card r1 s)) <$> [Hearts .. Spades]) <>
  ((\s -> (Card r1 s, Card r0 s)) <$> [Hearts .. Spades])
bToCards (Offsuited r0 r1) =
  (bimap (Card r0) (Card r1) <$>
    enum2 [Hearts .. Spades]) <>
  (bimap (Card r1) (Card r0) <$>
    enum2 [Hearts .. Spades])

-- | representation of each B as one pair of cards.
--
bRep :: B -> [Card]
bRep (Paired r) = [Card r Hearts, Card r Spades]
bRep (Offsuited r0 r1) = [Card r0 Hearts, Card r1 Spades]
bRep (Suited r0 r1) = [Card r0 Hearts, Card r1 Hearts]

-- | a sample of the probability of losing, given a B
--
-- >>> let xss = vsB 100 <$> (toEnum <$> [0..168])
-- >>> let awin = Strat $ fromList xss
--
vsB :: Int -> B -> Double
vsB n b = (\x -> fromIntegral (sum x) / fromIntegral (length x)) $
  flip evalState (mkStdGen 42) $ replicateM n $ do
    p1 <- dealN 2
    h <- dealN 5
    pure $ snd $ winner $ TableState [p0, p1] h
  where
    p0 = bRep b

-- | pixel chart of an array of Bs
--
-- >>> writeChartSvg "other/win.svg" $ stratText & #chartList %~ (\x -> bPixelChart (defaultSurfaceStyle & #surfaceColors .~ [Colour 0 1 0 1, Colour 1 0 0 1]) (defaultSurfaceLegendOptions $ pack "win") (Strat $ fromList $ vsB 100 <$> (toEnum <$> [0..168])) <> x)
--
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  Strat Double ->
  [Chart Double]
bPixelChart pixelStyle plo s =
  runHud (aspect 1) hs1 cs1
  where
    pts = Point 13 13
    gr :: Rect Double
    gr = Rect 0 13 0 13
    f :: Point Double -> Double
    f (Point x y) = index s (toEnum $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo
