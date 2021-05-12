{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Card entropy
module Poker.Random
  ( rvi,
    rvis,
    rviv,
    shuffle,
    vshuffle,
    dealN,
    dealNS,
    dealNWith,
    dealTable,
    dealHand,
    dealTableHand,
    rvs52,
    rvHandRank,
    card7s,
    card7sS,
    card7sSI,
    tables,
    tablesB,
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Lens.Micro
import NumHask.Prelude
import Poker.Types

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as Text

-- | uniform random variate of an Enum-style Int
--
-- >>> pretty (toEnum $ evalState (rvi 52) (mkStdGen 42) :: Card)
-- A♡
rvi :: (RandomGen g) => Int -> State g Int
rvi n = do
  g <- get
  let (x, g') = uniformR (0, n - 1) g
  put g'
  pure x

-- | reducing finite population n samples
--
-- >>> let xs = evalState (rvis 52 7) (mkStdGen 42)
-- >>> xs
-- [48,23,31,15,16,18,17]
--
-- evalState (shuffle 52 <$> (rvis 52 7)) (mkStdGen 42)
-- ([48,23,32,15,17,20,19],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,21,22,24,25,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,49,50,51])
rvis :: (RandomGen g) => Int -> Int -> State g [Int]
rvis n k = sequence (rvi . (n -) <$> [0 .. (k - 1)])

-- | finite population n samples without replacement
--
-- >>> evalState (rviv 52 7) (mkStdGen 42)
-- [48,23,31,15,16,18,17]
--
-- >>> evalState (vshuffle <$> (rviv 52 7)) (mkStdGen 42)
-- [48,23,32,15,17,20,19]
rviv :: (RandomGen g) => Int -> Int -> State g (S.Vector Int)
rviv n k = S.mapM (rvi . (n -)) (S.generate k id)

-- | a valid series of random index values to shuffle a population of 52 enums (for testing)
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

-- | deal n cards from a fresh, shuffled, standard pack.
--
-- >>> pretty $ evalState (dealN 7) (mkStdGen 42)
-- A♡7♠T♡5♠6♣7♡6♠
dealN :: (RandomGen g) => Int -> State g [Card]
dealN n = fmap toEnum . ishuffle <$> rvis 52 n

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
-- > shuffle 52 (take 52 rvs52) == ishuffle rvs52
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
        x1 = foldl' (\acc d -> bool acc (acc + one) (d <= acc)) (S.unsafeHead as) (sort $ S.toList dealt)

-- | deal n cards as a CardsS
--
-- >>> pretty $ evalState (dealNS 7) (mkStdGen 42)
-- A♡7♠T♡5♠6♣7♡6♠
dealNS :: (RandomGen g) => Int -> State g CardsS
dealNS n = CardsS . S.map fromIntegral . vshuffle <$> rviv 52 n

-- >>> pretty $ evalState (dealNSWith 7 deckS) (mkStdGen 42)
-- A♡7♠T♡5♠6♣7♡6♠
dealNWith :: (RandomGen g) => Int -> CardsS -> State g CardsS
dealNWith n (CardsS cs) = fmap (CardsS . S.map (cs S.!) . vshuffle) (rviv (S.length cs) n)

-- | deal n cards given a Hand has been dealt.
--
-- >>> pretty $ evalState (dealHand (Paired Ace) 7) (mkStdGen 42)
-- A♣7♠T♡5♠6♣7♡6♠
dealHand :: (RandomGen g) => Hand -> Int -> State g CardsS
dealHand b n =
  dealNWith n .
  (\(x,y) -> let (x',y') =
                  bool (y,x) (x,y) (x <= y) in
              S.splitAt x' (uncardsS deckS) &
              second (S.splitAt (y'-x')) &
              (\(t,(t', t'')) -> t <> S.tail t' <> S.tail t'') &
              CardsS
  ) .
  bimap (fromIntegral . fromEnum) (fromIntegral . fromEnum) .
  toRepPair $
  b

-- | deal a table
--
-- >>> pretty $ evalState (dealTable defaultTableConfig) (mkStdGen 42)
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
dealTable :: (RandomGen g) => TableConfig -> State g Table
dealTable cfg = do
  cs <- dealNS (5 + cfg ^. #numPlayers * 2)
  pure $ makeTable cfg (riso cardsS cs)

-- | deal a table given player i has been dealt a B
--
-- >>> pretty $ evalState (dealTableHand defaultTableConfig 0 (Paired Ace)) (mkStdGen 42)
-- A♡A♠ A♣7♠,T♡5♠6♣7♡6♠,hero: Just 0,o o,9.5 9,0.5 1,0,
dealTableHand :: (RandomGen g) => TableConfig -> Int -> Hand -> State g Table
dealTableHand cfg i b = do
  (CardsS xs) <- dealHand b (5 + (cfg ^. #numPlayers - 1) * 2)
  pure $
    makeTableS cfg $ CardsS $
    S.take (2 * i) xs <>
    (\(x,y) -> uncardsS $ liso cardsS [x,y]) (toRepPair b) <>
    S.drop (2 * i) xs

-- | uniform random variate of HandRank
--
-- >>> evalState rvHandRank (mkStdGen 42)
-- HighCard King Ten Nine Three Two
rvHandRank :: (RandomGen g) => State g HandRank
rvHandRank = do
  g <- get
  let (x, g') = uniformR (0, V.length allHandRanksV - 1) g
  put g'
  pure (allHandRanksV V.! x)

-- * random card generation

-- | random 7-Card list of lists
--
-- >>> sequence_ $ pretty <$> card7s 2
-- A♡7♠T♡5♠6♣7♡6♠
-- 7♠4♠T♣3♣6♡K♢T♢
card7s :: Int -> [[Card]]
card7s n = evalState (replicateM n (fmap toEnum . ishuffle <$> rvis 52 7)) (mkStdGen 42)

-- | Flat storable vector of n 7-card sets.
--
-- >>> S.length $ uncards2S $ card7sS 100
-- 700
card7sS :: Int -> Cards2S
card7sS n = Cards2S $ S.convert $ S.map fromIntegral $ mconcat $
  evalState
  (replicateM n (vshuffle <$> rviv 52 7))
  (mkStdGen 42)

-- | flat storable vector of ints, representing n 7-card sets
--
-- uses ishuffle
card7sSI :: Int -> Cards2S
card7sSI n = Cards2S $ S.fromList $ fmap fromIntegral $ mconcat $
  evalState
  (replicateM n (ishuffle <$> rvis 52 7))
  (mkStdGen 42)

-- | create a list of n dealt tables, with p players
--
-- sequence_ $ pretty <$> tables 2 2
-- A♡7♠ T♡5♠,6♣7♡6♠9♡4♠,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 9♠A♠ 3♣5♠,K♡T♣9♢9♣2♢,hero: Just 0,o o,9.5 9,0.5 1,0,
tables :: Int -> Int -> [Table]
tables p n =
  evalState
  (replicateM n
   (dealTable (defaultTableConfig & #numPlayers .~ p)))
  (mkStdGen 42)

-- | create a list of n dealt tables, with p players, where b is dealt to player k
--
-- >>> sequence_ $ pretty <$> tablesB 2 (Paired Ace) 1 3
-- A♣7♠ A♡A♠,T♡5♠6♣7♡6♠,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 7♠4♠ A♡A♠,T♣3♣6♡K♢T♠,hero: Just 0,o o,9.5 9,0.5 1,0,
-- 9♡8♠ A♡A♠,K♠2♢4♢5♣T♡,hero: Just 0,o o,9.5 9,0.5 1,0,
--
tablesB :: Int -> Hand -> Int -> Int -> [Table]
tablesB p b k n =
  evalState
  (replicateM n
   (dealTableHand (defaultTableConfig & #numPlayers .~ p) k b))
  (mkStdGen 42)
