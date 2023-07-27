{-# LANGUAGE OverloadedStrings #-}

-- | Card entropy
module Poker.Random
  ( -- * Usage
    -- $usage

    -- * Sampling
    rvi,
    rviv,
    rvis,
    cutShuffle,
    indexShuffle,

    -- * Deals
    dealN,
    dealNWith,
    dealTable,
    rvHandRank,

    -- * Random Card sets
    card7sS,
    card7sSI,

    -- * Enumeration
    enum2,
  )
where

import Control.Monad
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
  )
import Data.Bool (bool)
import Data.Foldable (Foldable (foldl'))
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Word
import Poker.Card.Storable
import Poker.HandRank
import Poker.Table
import System.Random (RandomGen, UniformRange, mkStdGen, uniformR)
import Prelude

-- $setup
-- >>> import Control.Monad.State.Lazy
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Poker.Range
-- >>> import Optics.Core
-- >>> import Data.Word
-- >>> import Poker.Table
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import System.Random
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Vector.Storable as S
-- >>> let eval = flip evalState (mkStdGen 69)

-- | Uniform random variate of an Int
--
-- @rvi 52@ generates a random variate between 0 and 51 inclusive.
--
-- >>> eval $ rvi 52
-- 5
rvi :: (RandomGen g, Num e, UniformRange e) => e -> State g e
rvi n = do
  g <- get
  let (x, g') = uniformR (0, n - 1) g
  put g'
  pure x
{-# INLINE rvi #-}

-- | reducing finite population n samples
--
-- @rvis 52 2@ produces a list containing a random variate between 0 and 51, and an rv between 0 and 50.
--
-- >>> eval $ rviv 52 7 :: S.Vector Word8
-- [36,20,11,33,43,13,34]
rviv :: (RandomGen g, UniformRange e, Integral e, S.Storable e) => e -> e -> State g (S.Vector e)
rviv n k = S.mapM (rvi . (n -)) (S.generate (fromIntegral k) fromIntegral)
{-# INLINE rviv #-}

rvis :: (RandomGen g, UniformRange e, Num e, Enum e) => e -> e -> State g [e]
rvis n k = mapM (rvi . (n -)) [0 .. (k - 1)]
{-# INLINE rvis #-}

-- | Creates sample without replacement given an 'rvis' process.
--
-- Does this by actually cutting up vectors.
--
-- >>> rvs52 = eval $ rvis 52 52
-- >>> cutShuffle 52 rvs52
-- ([5,6,34,30,21,40,3,17,35,29,36,13,50,14,0,39,1,11,12,22,25,45,38,49,2,46,48,33,24,42,15,10,18,37,28,27,9,19,23,47,16,20,8,51,26,44,4,31,7,32,43,41],[])
cutShuffle :: Int -> [Int] -> (V.Vector Int, V.Vector Int)
cutShuffle n =
  foldl'
    ( \(dealt, r) i ->
        let (x, rem') = cutV r i in (V.snoc dealt x, rem')
    )
    (V.empty, V.enumFromN 0 n)
{-# INLINE cutShuffle #-}

-- | cut a vector at n, returning the n'th element, and the truncated vector
cutV :: V.Vector a -> Int -> (a, V.Vector a)
cutV v x =
  ( v V.! x,
    V.unsafeSlice 0 x v <> V.unsafeSlice (x + 1) (n - x - 1) v
  )
  where
    n = V.length v
{-# INLINE cutV #-}

-- | Creates sample without replacement given an 'rvis' process.
--
-- Computation treats rvis output as indices.
--
-- isomorphic to fst . shuffle 52
--
-- >>> rvs52 = flip evalState (mkStdGen 42) $ rvis 52 52
-- >>> indexShuffle rvs52
-- [48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42]
--
-- TODO: refactor the sort
indexShuffle :: (Ord e, Num e) => [e] -> [e]
indexShuffle as = reverse $ go as []
  where
    go [] s = s
    go (x0 : xs) s = go xs (x1 : s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + 1) (d <= acc)) x0 (List.sort s)
{-# INLINE indexShuffle #-}

-- | deal n cards as a CardsS
--
-- > pretty (view cardsI) $ evalState (dealN 7) (mkStdGen 42)
-- Ac7sTc5s6d7c6s
dealN :: (RandomGen g) => Word8 -> State g CardsS
dealN n = CardsS . S.fromList . indexShuffle . S.toList <$> rviv 52 n
{-# INLINE dealN #-}

-- | deal n cards from a given deck
--
-- >>> pretty $ evalState (dealNWith 7 allCardsS) (mkStdGen 42)
-- Js2h9s6s8c5sQh
dealNWith :: (RandomGen g) => Word8 -> CardsS -> State g CardsS
dealNWith n (CardsS cs) = fmap (CardsS . S.map (cs S.!) . S.fromList . fmap fromIntegral . indexShuffle . S.toList) (rviv (fromIntegral $ S.length cs) n)
{-# INLINE dealNWith #-}

-- | deal a table
--
-- >>> pretty $ evalState (dealTable defaultTableConfig) (mkStdGen 42)
-- Js2h 9s6s|8c5sQh|5c|6c,hero: 0,o o,9.5 9.0,0.50 1.0,0.0,
dealTable :: (RandomGen g) => TableConfig -> State g Table
dealTable cfg = do
  cs <- dealN (toEnum $ 5 + tableSize cfg * 2)
  pure $ makeTable cfg cs
{-# INLINE dealTable #-}

-- | uniform random variate of HandRank
--
-- >>> pretty $ evalState rvHandRank (mkStdGen 42)
-- HighCard King Ten Nine Three Two
rvHandRank :: (RandomGen g) => State g HandRank
rvHandRank = do
  g <- get
  let (x, g') = uniformR (0, length allHandRanks - 1) g
  put g'
  pure (allHandRanks List.!! x)

-- | Flat storable vector of n 7-card sets.
--
-- >>> S.length $ unwrapCards2S $ card7sS 100
-- 700
card7sS :: Int -> Cards2S
card7sS n =
  Cards2S $
    S.convert $
      mconcat $
        evalState
          (replicateM n (S.fromList . indexShuffle . S.toList <$> rviv 52 7))
          (mkStdGen 42)
{-# INLINE card7sS #-}

-- | flat storable vector of ints, representing n 7-card sets using ishuffle
--
-- >>> S.length $ unwrapCards2S $ card7sSI 100
-- 700
card7sSI :: Int -> Cards2S
card7sSI n = Cards2S $ S.concat $ V.toList $ evalState (V.replicateM n (unwrapCardsS <$> dealN 7)) (mkStdGen 42)
{-# INLINE card7sSI #-}

-- | An enumeration of 2 samples from a list without replacement
--
-- >>> enum2 [0..2]
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
enum2 :: [a] -> [(a, a)]
enum2 xs = fmap (p . fmap toEnum) . (\x y -> indexShuffle [x, y]) <$> [0 .. (n - 1)] <*> [0 .. (n - 2)]
  where
    n = length xs
    p (x : y : _) = (xs List.!! x, xs List.!! y)
    p _ = error "list too short"
{-# INLINE enum2 #-}
