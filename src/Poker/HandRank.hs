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
module Poker.HandRank
  ( HandRank(..),
    handRank,
    straight,
    flush,
    rank,
  ) where

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

-- | compute a HandRank from a list of Cards.
--
-- Should work for 5 and 7 hand variants.
--
--
handRank :: [Card] -> Maybe HandRank
handRank cs =
  flush cs' <|>
  straight cs' <|>
  kind cs'
  where
    cs' = sortOn Down cs

-- | check for a straight
straight :: [Card] -> Maybe HandRank
straight cs = Straight <$> str8 (toList $ ranks cs)

rangeCount :: (Enum a) => Mealy a [(a,a)]
rangeCount = M (\a -> ((a,a),[])) (\((rmin,rmax), rs) a -> bool ((a,a),(rmin,rmax):rs) ((rmin,a),rs) (fromEnum a == fromEnum rmax + one)) (\x -> fst x:snd x)

str8 :: Enum a => [a] -> Maybe a
str8 xs = case runs of
  [] -> Nothing
  ((_,r):_) -> Just r
  where
    runs = filter (\(x,y) -> fromEnum y - fromEnum x >= 4) (Data.Mealy.fold rangeCount xs)

-- | check for a flush
flush :: [Card] -> Maybe HandRank
flush cs =
  case filter ((>=5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_,rs@(r0:r1:r2:r3:r4:_)):_) ->
      Just $
      maybe
      (Flush r0 r1 r2 r3 r4)
      StraightFlush
      (str8 rs)
    _ -> Nothing

suitRanks :: [Card] -> [(Suit, [Rank])]
suitRanks cs =
  Map.toList $
  Map.fromDescListWith (<>) $
  fmap (\(Card r s) -> (s,[r])) cs

rankCount :: [Card] -> [(Rank,Int)]
rankCount cs =
  sortOn (Down . swap) $
  Map.toList $
  Map.fromDescListWith (+) $
  fmap (\(Card r _) -> (r,1)) cs

-- | check for the highest-kinded hand that can be made.
kind :: [Card] -> Maybe HandRank
kind cs = case rankCount cs of
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

-- $performance
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
