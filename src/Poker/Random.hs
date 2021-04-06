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
module Poker.Random where

import Poker.Types
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

-- $setup
--
-- >>> :set -XOverloadedStrings

-- | uniform random variate of an Enum-style Int
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

-- | deal n cards from a fresh, shuffled, standard pack.
--
-- >>> putStrLn $ Text.intercalate "\n" $ fmap short <$> flip evalState (mkStdGen 44) $ replicateM 5 (dealN 5)
-- A♣3♠K♠7♡9♠
-- 9♠7♡2♣Q♢J♣
-- K♢4♣9♢K♠7♠
-- 7♣7♠J♡8♡J♢
-- 5♠Q♣A♣Q♡T♠
--
dealN :: (RandomGen g) => Int -> State g [Card]
dealN n = fmap toEnum . ishuffle <$> rvis 52 n

dealNWith :: (RandomGen g) => Int -> [Card] -> State g [Card]
dealNWith n cs = fmap (cs List.!!) . ishuffle <$> rvis (length cs) n

-- | deal a random table
--
dealTable :: (RandomGen g) => TableConfig -> State g TableState
dealTable cfg = do
  cs <- dealN (5 + 2 * cfg ^. #numPlayers)
  pure $ table0 cfg cs

-- | deal n cards given a B has been dealt.
dealB :: (RandomGen g) => B -> Int -> State g [Card]
dealB b n = dealNWith n (deck List.\\ (\(x,y)->[x,y]) (btoc b))

-- | deal a table given player i has been dealt a B
dealTableB :: (RandomGen g) => TableConfig -> Int -> B -> State g TableState
dealTableB cfg i b = do
  cs <- dealB b (5+(cfg ^. #numPlayers - 1)*2)
  pure $ table0 cfg (take (2*i) cs <> (\(x,y)->[x,y]) (btoc b) <> drop (2*i) cs)
