{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Storable version of 'Poker.Card' and related types contained in "Poker".
--
-- In general, this module provides:
--
-- - 'Storable' and 'Vector' versions of types in poker-base
--
-- - an 'Iso' conversion, named after the type and suffixed with an S.
module Poker.Card.Storable
  ( -- * Usage
    -- $usage

    -- * Card
    Rank (..),
    rank,
    Ranks (..),
    Suit (..),
    suit,
    Suits (..),
    Card (..),
    allCards,
    toRank,
    toSuit,
    Cards (..),
    ranksSet,
    toRanks,
    Cards2 (..),
    cardsS7V,

    -- * Vector application
    applyFlat,
    applyFlatV,
    apply,
    applyV,
  )
where

import Control.DeepSeq
import Data.Foldable
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import Data.Word
import Optics.Core
import Poker hiding (Card, Rank, Suit, allCards, fromList)
import Prettyprinter hiding (comma)
import Prelude

-- $usage
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> let cs = [Card Ace Heart,Card Seven Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> :t from cards cs
-- from cards cs :: CardsS
--
-- >>> pretty $ from cards cs
-- Ah7sTh5s6c7h6s
--
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]

-- $setup
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> let cs = [Card Ace Heart,Card Seven Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> :t from cards cs
-- from cards cs :: CardsS
--
-- >>> pretty $ from cards cs
-- Ah7sTh5s6c7h6s
--
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]

-- | Type to support bidirectional conversion between poker-base data structures and 'Data.Vector.Storable' types.

--

-- | Storable representation of 'Rank'
--
-- >>> to rank $ Rank 0
-- Two
-- >>> to rank $ Rank 12
-- Ace
newtype Rank = Rank {unwrapRank :: Word8} deriving (Eq, Show, Ord, NFData)

-- | Storable representation of 'Rank'
--
-- >>> from ranks allRanks
-- Ranks {unwrapRanks = [0,1,2,3,4,5,6,7,8,9,10,11,12]}
--
-- >>> pretty $ from ranks allRanks
-- 23456789TJQKA
newtype Ranks = Ranks {unwrapRanks :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

-- | Storable representation of 'Suit'
--
-- >>> to suit $ Suit 0
-- Club
-- >>> to suit $ Suit 3
-- Spade
newtype Suit = Suit {unwrapSuit :: Word8} deriving (Eq, Show, Ord, NFData)

-- | Storable representation of 'Suit'
--
-- >>> from suits allSuits
-- Ranks {unwrapRanks = [0,1,2,3,4,5,6,7,8,9,10,11,12]}
--
-- >>> pretty $ from ranks allRanks
-- 23456789TJQKA
newtype Suits = Suits {unwrapSuits :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

-- | Storable representation of a 'Card'
--
-- >>> to card $ Card 0
-- Card {rank = Two, suit = Club}
-- >>> to card $ Card 51
-- Card {rank = Ace, suit = Spade}
newtype Card = Card {unwrapCard :: Word8} deriving (Eq, Show, Ord)

-- | Storable representation of ['Card'].
--
-- >>> pretty $ from cards cs
-- Ah7sTh5s6c7h6s
newtype Cards = Cards {unwrapCards :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

-- | a standard 52 card deck (in ascending order).
--
-- >>> pretty allCardsS
-- 2c2d2h2s3c3d3h3s4c4d4h4s5c5d5h5s6c6d6h6s7c7d7h7s8c8d8h8s9c9d9h9sTcTdThTsJcJdJhJsQcQdQhQsKcKdKhKsAcAdAhAs
allCards :: Cards
allCards = Cards $ S.fromList [0 .. 51]

-- | Extract rank from a CardS.
--
-- >>> pretty $ toRank (Card 0)
-- 2
toRank :: Card -> Rank
toRank (Card c) = Rank $ c `div` 4

-- | Extract suit from a CardS.
--
-- >>> pretty $ toSuit (Card 0)
-- c
toSuit :: Card -> Suit
toSuit (Card c) = Suit $ c `mod` 4

-- | Ranks in a hand (with no duplicates)
--
-- >>> pretty $ ranksSet (from cards cs)
-- AT765
ranksSet :: Cards -> Ranks
ranksSet (Cards xs) =
  Ranks $
    S.fromList $
      Set.toDescList $
        Set.fromList $
          S.toList $
            S.map (unwrapRank . toRank . Card) xs

-- | Ranks of a hand (with duplicates)
--
-- >>> pretty $ toRanks (from cards cs)
-- A7T5676
toRanks :: Cards -> Ranks
toRanks cs = Ranks $ S.map (unwrapRank . toRank . Card) $ unwrapCards cs

-- | Storable representation of a [['Card']]
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]
newtype Cards2 = Cards2 {unwrapCards2 :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

instance Semigroup Cards2 where
  (<>) (Cards2 x) (Cards2 x') = Cards2 (x <> x')

instance Monoid Cards2 where
  mempty = Cards2 S.empty

-- | Convert between a 'Cards2S' and a boxed vector of 'CardsS'.
--
-- The main purpose of this representation is to access vector operations for things that aren't storable.
--
-- >>> (\x -> (to cardsS7V $ from cardsS7V x) == x) (from cardsS7L css)
-- True
cardsS7V :: Iso' Cards2 (V.Vector Cards)
cardsS7V = iso (applyFlatV 7 Cards . unwrapCards2) (Cards2 . fold . V.map unwrapCards)

-- | Apply a function that takes a vector by slicing the supplied main vector n times.
--
-- >>> V.toList $ applyFlatV 7 (pretty . CardsS) (unwrapCards2 $ (from cardsS7L css))
-- [Ah7sTh5s6c7h6s,Tc5sTh5s6c7h6s]
applyFlatV :: (Storable s) => Int -> (S.Vector s -> a) -> S.Vector s -> V.Vector a
applyFlatV k f s = V.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable for each slice.
--
-- >>> applyFlat 7 S.length (unwrapCards2 $ (from cardsS7L css))
-- [7,7]
applyFlat :: (Storable s, Storable a) => Int -> (S.Vector s -> a) -> S.Vector s -> S.Vector a
applyFlat k f s = S.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | apply a function to a cards vector, returning a boxed vector of the results.
--
-- >>> applyV (pretty . ranksSet) (from cardsS7L css)
-- [AT765,T765]
applyV :: (Cards -> a) -> Cards2 -> V.Vector a
applyV f (Cards2 s) = applyFlatV 7 (f . Cards) s
{-# INLINE applyV #-}

-- | apply a function to a cards vector, returning a storable vector of the results.
--
-- >>> apply (S.length . unwrapCards) (from cardsS7L css)
-- [7,7]
apply :: (Storable a) => (Cards -> a) -> Cards2 -> S.Vector a
apply f (Cards2 s) = applyFlat 7 (f . Cards) s
