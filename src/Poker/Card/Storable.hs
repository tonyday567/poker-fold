{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
--
module Poker.Card.Storable
  (
    -- * Usage
    -- $usage

    -- * Isomorphisms
    Iso (..),

    -- * Card
    RankS (..),
    rankS,
    RanksS (..),
    ranksS,
    SuitS (..),
    suitS,
    SuitsS,
    suitsS,
    ranks,
    suits,
    CardS (..),
    cardS,
    allCardsS,
    toRankS,
    toSuitS,
    CardsS (..),
    cardsS,
    ranksSet,
    toRanksS,
    Cards2S (..),
    cardsS7V,
    cardsS7L,

    -- * Vector application
    applyFlatV,
    applyFlatS,
    applyV,
    applyS,
  )
where

import Data.Foldable
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import Data.Word
import Poker hiding (fromList)
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
-- >>> :t from cardsS cs
-- from cardsS cs :: CardsS
--
-- >>> pretty $ from cardsS cs
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
-- >>> :t from cardsS cs
-- from cardsS cs :: CardsS
--
-- >>> pretty $ from cardsS cs
-- Ah7sTh5s6c7h6s
--
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]

-- | Type to support bidirectional conversion between poker-base data structures and 'Data.Vector.Storable' types.
--
-- This module is experimental and the API is subject to change if faster or safer methods are discovered.
--
-- For conversion between representations, 'Enum' is a bit tricky. Enum instances do not support list fusion eg see <https://gitlab.haskell.org/ghc/ghc/-/issues/18178>.
--
-- Conversion routines here, as a consequence, avoid them and encode direct methods.
--
-- Note that conversion from Integral representations provided here to 'Poker' sum-types are partial. Blame Hask -> Hask, if you must.
--
-- > from . to == id
-- > to . from == id
data Iso a b = Iso {from :: a -> b, to :: b -> a}

-- | Storable representation of 'Rank'
--
-- >>> to rankS $ RankS 0
-- Two
-- >>> to rankS $ RankS 12
-- Ace
newtype RankS = RankS {unwrapRank :: Word8} deriving (Eq, Show, Ord)

-- | isomorphism between 'Rank' and 'RankS'
--
rankS :: Iso Rank RankS
rankS = Iso fromRank toRank

toRank :: RankS -> Rank
toRank (RankS 0) = Two
toRank (RankS 1) = Three
toRank (RankS 2) = Four
toRank (RankS 3) = Five
toRank (RankS 4) = Six
toRank (RankS 5) = Seven
toRank (RankS 6) = Eight
toRank (RankS 7) = Nine
toRank (RankS 8) = Ten
toRank (RankS 9) = Jack
toRank (RankS 10) = Queen
toRank (RankS 11) = King
toRank (RankS 12) = Ace
toRank _ = undefined

fromRank :: Rank -> RankS
fromRank Two = RankS 0
fromRank Three = RankS 1
fromRank Four = RankS 2
fromRank Five = RankS 3
fromRank Six = RankS 4
fromRank Seven = RankS 5
fromRank Eight = RankS 6
fromRank Nine = RankS 7
fromRank Ten = RankS 8
fromRank Jack = RankS 9
fromRank Queen = RankS 10
fromRank King = RankS 11
fromRank Ace = RankS 12

instance Pretty RankS where
  pretty = pretty . to rankS

-- | Storable representation of ['Rank']
--
-- >>> from ranksS allRanks
-- RanksS {unwrapRanks = [0,1,2,3,4,5,6,7,8,9,10,11,12]}
--
-- >>> pretty $ from ranksS allRanks
-- 23456789TJQKA
newtype RanksS = RanksS {unwrapRanks :: S.Vector Word8} deriving (Eq, Show, Ord)

-- | Conversion between rank traversables
ranksS :: Iso [Rank] RanksS
ranksS =
  Iso
    (RanksS . S.fromList . fmap (unwrapRank . from rankS))
    (fmap (to rankS . RankS) . S.toList . unwrapRanks)

instance Pretty RanksS where
  pretty = mconcat . fmap pretty . to ranksS

-- | Storable representation of 'Suit'
--
-- >>> to suitS $ SuitS 0
-- Club
-- >>> to suitS $ SuitS 3
-- Spade
newtype SuitS = SuitS {unwrapSuit :: Word8} deriving (Eq, Show, Ord)

-- | isomorphism between 'Suit' and 'SuitS'
--
suitS :: Iso Suit SuitS
suitS = Iso fromSuit toSuit

toSuit :: SuitS -> Suit
toSuit (SuitS 0) = Club
toSuit (SuitS 1) = Diamond
toSuit (SuitS 2) = Heart
toSuit (SuitS 3) = Spade
toSuit _ = undefined

fromSuit :: Suit -> SuitS
fromSuit Club = SuitS 0
fromSuit Diamond = SuitS 1
fromSuit Heart = SuitS 2
fromSuit Spade = SuitS 3

instance Pretty SuitS where
  pretty = pretty . to suitS

-- | Storable representation of ['Suit']
--
-- >>> from suitsS allSuits
-- SuitsS {unwrapSuits = [0,1,2,3]}
--
-- >>> pretty $ from suitsS allSuits
-- cdhs
newtype SuitsS = SuitsS {unwrapSuits :: S.Vector Word8} deriving (Eq, Show, Ord)

-- | Conversion between suit traversables
suitsS :: Iso [Suit] SuitsS
suitsS =
  Iso
    (SuitsS . S.fromList . fmap (unwrapSuit . from suitS))
    (fmap (to suitS . SuitS) . S.toList . unwrapSuits)

instance Pretty SuitsS where
  pretty = mconcat . fmap pretty . to suitsS

-- | Set of ranks in a ['Card'] (with no duplicates)
--
-- >>> ranks cs
-- fromList [Five,Six,Seven,Ten,Ace]
ranks :: [Card] -> Set.Set Rank
ranks cs = Set.fromList $ rank <$> cs

-- | Set of suits in a hand (with no duplicates)
--
-- >>> suits cs
-- fromList [Club,Heart,Spade]
suits :: [Card] -> Set.Set Suit
suits cs = Set.fromList $ suit <$> cs

-- | Storable representation of a 'Card'
--
-- >>> to cardS $ CardS 0
-- Card {rank = Two, suit = Club}
-- >>> to cardS $ CardS 51
-- Card {rank = Ace, suit = Spade}
newtype CardS = CardS {unwrapCard :: Word8} deriving (Eq, Show, Ord)

-- | card conversion
cardS :: Iso Card CardS
cardS = Iso fromCard toCard

toCard :: CardS -> Card
toCard (CardS x) = let (r, s) = x `divMod` 4 in Card (toRank (RankS r)) (toSuit (SuitS s))

fromCard :: Card -> CardS
fromCard (Card r s) = CardS $ unwrapRank (fromRank r) * 4 + unwrapSuit (fromSuit s)

instance Pretty CardS where
  pretty = pretty . to cardS

-- | Storable representation of ['Card'].
--
-- >>> pretty $ from cardsS cs
-- Ah7sTh5s6c7h6s
newtype CardsS = CardsS {unwrapCards :: S.Vector Word8} deriving (Eq, Show, Ord)

-- | Conversion between traversable card types
cardsS :: Iso [Card] CardsS
cardsS =
  Iso
    (CardsS . S.fromList . fmap (unwrapCard . from cardS))
    (fmap (to cardS . CardS) . S.toList . unwrapCards)

instance Pretty CardsS where
  pretty = mconcat . fmap pretty . to cardsS

-- | a standard 52 card deck (in ascending order).
--
-- >>> pretty allCardsS
-- 2c2d2h2s3c3d3h3s4c4d4h4s5c5d5h5s6c6d6h6s7c7d7h7s8c8d8h8s9c9d9h9sTcTdThTsJcJdJhJsQcQdQhQsKcKdKhKsAcAdAhAs
allCardsS :: CardsS
allCardsS = CardsS $ S.fromList [0 .. 51]

-- | Extract rank from a CardS.
--
-- >>> pretty $ toRankS (CardS 0)
-- 2
toRankS :: CardS -> RankS
toRankS (CardS c) = RankS $ c `div` 4

-- | Extract suit from a CardS.
--
-- >>> pretty $ toSuitS (CardS 0)
-- c
toSuitS :: CardS -> SuitS
toSuitS (CardS c) = SuitS $ c `mod` 4

-- | Ranks in a hand (with no duplicates)
--
-- >>> pretty $ ranksSet (from cardsS cs)
-- AT765
ranksSet :: CardsS -> RanksS
ranksSet (CardsS xs) =
  RanksS $
    S.fromList $
      Set.toDescList $
        Set.fromList $
          S.toList $
            S.map (unwrapRank . toRankS . CardS) xs

-- | Ranks of a hand (with duplicates)
--
-- >>> pretty $ toRanksS (from cardsS cs)
-- A7T5676
toRanksS :: CardsS -> RanksS
toRanksS cs = RanksS $ S.map (unwrapRank . toRankS . CardS) $ unwrapCards cs

-- | Storable representation of a [['Card']]
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]
newtype Cards2S = Cards2S {unwrapCards2 :: S.Vector Word8} deriving (Eq, Show, Ord)

instance Semigroup Cards2S where
  (<>) (Cards2S x) (Cards2S x') = Cards2S (x <> x')

instance Monoid Cards2S where
  mempty = Cards2S S.empty

instance Pretty Cards2S where
  pretty = pretty . V.toList . applyV id

-- | Convert between a list of 7 card lists and a 'Cards2S'
--
-- >>> (\x -> (to cardsS7L $ from cardsS7L x) == x) css
-- True
cardsS7L :: Iso [[Card]] Cards2S
cardsS7L =
  Iso
    (Cards2S . S.fromList . fmap (toEnum . fromEnum) . mconcat)
    (V.toList . applyFlatV 7 (fmap (toEnum . fromEnum) . S.toList) . unwrapCards2)

-- | Convert between a 'Cards2S' and a boxed vector of 'CardsS'.
--
-- The main purpose of this representation is to access vector operations for things that aren't storable.
--
-- >>> (\x -> (to cardsS7V $ from cardsS7V x) == x) (from cardsS7L css)
-- True
cardsS7V :: Iso Cards2S (V.Vector CardsS)
cardsS7V = Iso (applyFlatV 7 CardsS . unwrapCards2) (Cards2S . fold . V.map unwrapCards)

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
-- >>> applyFlatS 7 S.length (unwrapCards2 $ (from cardsS7L css))
-- [7,7]
applyFlatS :: (Storable s, Storable a) => Int -> (S.Vector s -> a) -> S.Vector s -> S.Vector a
applyFlatS k f s = S.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | apply a function to a cards vector, returning a boxed vector of the results.
--
-- >>> applyV (pretty . ranksSet) (from cardsS7L css)
-- [AT765,T765]
applyV :: (CardsS -> a) -> Cards2S -> V.Vector a
applyV f (Cards2S s) = applyFlatV 7 (f . CardsS) s

-- | apply a function to a cards vector, returning a storable vector of the results.
--
-- >>> applyS (S.length . unwrapCards) (from cardsS7L css)
-- [7,7]
applyS :: (Storable a) => (CardsS -> a) -> Cards2S -> S.Vector a
applyS f (Cards2S s) = applyFlatS 7 (f . CardsS) s
