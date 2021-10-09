{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Card entropy
module Poker.Random
  ( -- * Usage
    -- $usage

    -- * Sampling
    rvi,
    rvis,
    rviv,
    shuffle,
    ishuffle,
    vshuffle,

    -- * Deals
    dealN,
    dealNS,
    dealNWith,
    dealTable,
    rvHandRank,

    -- * Card sets
    card7s,
    card7sS,
    card7sSI,
    tables,

    -- * Enumeration
    enum2,
  )
where

import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
    replicateM,
  )
import Data.Bool (bool)
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Lens.Micro ((&), (.~), (^.))
import Poker
import Poker.Card.Storable
import Poker.Evaluate
import Poker.Table
import System.Random (RandomGen, mkStdGen, uniformR)
import Prelude

-- $usage
-- >>> import Control.Monad.State.Lazy
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import System.Random
-- >>> pretty $ evalState (dealNS 7) (mkStdGen 42)
-- Ac7sTc5s6d7c6s

-- $setup
-- >>> import Control.Monad.State.Lazy
-- >>> import Lens.Micro hiding (to)
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Evaluate
-- >>> import Poker.Random
-- >>> import Poker.RangedHand
-- >>> import Poker.Table
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import System.Random
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Vector.Storable as S

-- | Uniform random variate of an Int
--
-- @rvi 52@ generates a random variate between 0 and 51 inclusive.
--
-- >>> pretty (toEnum $ evalState (rvi 52) (mkStdGen 42) :: Card)
-- Ac
rvi :: (RandomGen g) => Int -> State g Int
rvi n = do
  g <- get
  let (x, g') = uniformR (0, n - 1) g
  put g'
  pure x

-- | reducing finite population n samples
--
-- @rvis 52 2@ produces a list containing a random variate between 0 and 51, and an rv between 0 and 50.
--
-- >>> let xs = evalState (rvis 52 7) (mkStdGen 42)
-- >>> xs
-- [48,23,31,15,16,18,17]
--
rvis :: (RandomGen g) => Int -> Int -> State g [Int]
rvis n k = sequence (rvi . (n -) <$> [0 .. (k - 1)])

-- | Vector version of rvis
--
-- >>> evalState (rviv 52 7) (mkStdGen 42)
-- [48,23,31,15,16,18,17]
--
rviv :: (RandomGen g) => Int -> Int -> State g (S.Vector Int)
rviv n k = S.mapM (rvi . (n -)) (S.generate k id)

-- | Creates sample without replacement given an 'rvis' process.
--
-- Does this by actually cutting up vectors.
--
-- >>> rvs52 = flip evalState (mkStdGen 42) $ rvis 52 52
-- >>> shuffle 52 rvs52
-- ([48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42],[])
shuffle :: Int -> [Int] -> (V.Vector Int, V.Vector Int)
shuffle n =
  foldl'
    ( \(dealt, rem) i ->
        let (x, rem') = cutV rem i in (V.snoc dealt x, rem')
    )
    (V.empty, V.enumFromN 0 n)

-- | cut a vector at n, returning the n'th element, and the truncated vector
cutV :: V.Vector a -> Int -> (a, V.Vector a)
cutV v x =
  ( v V.! x,
    V.unsafeSlice 0 x v <> V.unsafeSlice (x + 1) (n - x - 1) v
  )
  where
    n = V.length v

-- | Creates sample without replacement given an 'rvis' process.
--
-- Computation treats rvis output as indices.
--
-- isomorphic to fst . shuffle 52
--
-- >>> rvs52 = flip evalState (mkStdGen 42) $ rvis 52 52
-- >>> ishuffle rvs52
-- [48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42]
--
-- TODO: refactor the sort
ishuffle :: [Int] -> [Int]
ishuffle as = reverse $ go as []
  where
    go [] s = s
    go (x0 : xs) s = go xs (x1 : s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + 1) (d <= acc)) x0 (sort s)

-- | Vector version of ishuffle
--
-- >>> rvs52' = flip evalState (mkStdGen 42) $ rviv 52 52
-- >>> vshuffle rvs52'
-- [48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42]
--
-- >>> flip evalState (mkStdGen 42) $ fmap ((==[0..51]) . S.toList . sortS . vshuffle) (rviv 52 52)
-- True
vshuffle :: S.Vector Int -> S.Vector Int
vshuffle as = go as S.empty
  where
    go :: S.Vector Int -> S.Vector Int -> S.Vector Int
    go as dealt =
      bool
        (go (S.unsafeTail as) (S.snoc dealt x1))
        dealt
        (S.null as)
      where
        x1 = foldl' (\acc d -> bool acc (acc + 1) (d <= acc)) (S.unsafeHead as) (sort $ S.toList dealt)

-- | deal n cards from a fresh, shuffled, standard pack.
--
-- >>> pretty $ evalState (dealN 7) (mkStdGen 42)
-- [Ac, 7s, Tc, 5s, 6d, 7c, 6s]
dealN :: (RandomGen g) => Int -> State g [Card]
dealN n = fmap toEnum . ishuffle <$> rvis 52 n

-- | deal n cards as a CardsS
--
-- >>> pretty $ evalState (dealNS 7) (mkStdGen 42)
-- Ac7sTc5s6d7c6s
dealNS :: (RandomGen g) => Int -> State g CardsS
dealNS n = CardsS . S.map fromIntegral . vshuffle <$> rviv 52 n

-- | deal n cards from a given deck
--
-- >>> pretty $ evalState (dealNWith 7 allCardsS) (mkStdGen 42)
-- Ac7sTc5s6d7c6s
dealNWith :: (RandomGen g) => Int -> CardsS -> State g CardsS
dealNWith n (CardsS cs) = fmap (CardsS . S.map (cs S.!) . vshuffle) (rviv (S.length cs) n)

-- | deal a table
--
-- >>> pretty $ evalState (dealTable defaultTableConfig) (mkStdGen 42)
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
dealTable :: (RandomGen g) => TableConfig -> State g Table
dealTable cfg = do
  cs <- dealNS (5 + cfg ^. #numPlayers * 2)
  pure $ makeTable cfg (to cardsS cs)

-- | uniform random variate of HandRank
--
-- >>> evalState rvHandRank (mkStdGen 42)
-- HighCard King Ten Nine Three Two
rvHandRank :: (RandomGen g) => State g HandRank
rvHandRank = do
  g <- get
  let (x, g') = uniformR (0, length allHandRanks - 1) g
  put g'
  pure (allHandRanks List.!! x)

-- | random 7-Card list of lists
--
-- >>> pretty <$> card7s 2
-- [[Ac, 7s, Tc, 5s, 6d, 7c, 6s],[7s, 4s, Td, 3d, 6c, Kh, Ts]]
card7s :: Int -> [[Card]]
card7s n = evalState (replicateM n (fmap toEnum . ishuffle <$> rvis 52 7)) (mkStdGen 42)

-- | Flat storable vector of n 7-card sets.
--
-- >>> S.length $ unwrapCards2 $ card7sS 100
-- 700
card7sS :: Int -> Cards2S
card7sS n =
  Cards2S $
    S.convert $
      S.map fromIntegral $
        mconcat $
          evalState
            (replicateM n (vshuffle <$> rviv 52 7))
            (mkStdGen 42)

-- | flat storable vector of ints, representing n 7-card sets using ishuffle
--
-- >>> S.length $ unwrapCards2 $ card7sSI 100
-- 700
card7sSI :: Int -> Cards2S
card7sSI n =
  Cards2S $
    S.fromList $
      fmap fromIntegral $
        mconcat $
          evalState
            (replicateM n (ishuffle <$> rvis 52 7))
            (mkStdGen 42)

-- | create a list of n dealt tables, with p players
--
-- >>> pretty $ tables 2 2
-- [ Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
-- , 9sAs 3d5s|KcTd9h|9d|2h,hero: 0,o o,9.5 9,0.5 1,0, ]
tables :: Int -> Int -> [Table]
tables p n =
  evalState
    ( replicateM
        n
        (dealTable (defaultTableConfig & #numPlayers .~ p))
    )
    (mkStdGen 42)

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (p . fmap toEnum) . (\x y -> ishuffle [x, y]) <$> [0 .. (n - 1)] <*> [0 .. (n - 2)]
  where
    n = length xs
    p (x : y : _) = (xs List.!! x, xs List.!! y)
    p _ = error "list too short"

