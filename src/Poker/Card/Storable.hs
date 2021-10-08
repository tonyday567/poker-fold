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

-- | Storable versions of Poker.Card
module Poker.Card.Storable
  ( -- * basic card types
    RankS (..),
    rankS,
    RanksS (..),
    SuitS (..),
    suitS,
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
    applyFlat,
    applyFlatS,
    applyFlatM,
    applyV,
    applyS,
    applyM,

    -- * infrastructure
    Iso (..),
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

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Poker
-- >>> import Poker.Random
-- >>> import Poker.Table
-- >>> import Prettyprinter
-- >>> import qualified Data.List as List
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> let cs = [Card {rank = Ace, suit = Heart},Card {rank = Seven, suit = Spade},Card {rank = Ten, suit = Heart},Card {rank = Five, suit = Spade},Card {rank = Six, suit = Club},Card {rank = Seven, suit = Heart},Card {rank = Six, suit = Spade},Card {rank = Nine, suit = Heart},Card {rank = Four, suit = Spade}]
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- Hero raises and quick fold from the BB.

-- | iso's for main type class structure
--
-- Types are provided for two different contexts. Where naming & enumeration of basic types aids in clarity, such as text conversion and reporting, we provide primitive types and Enum instances to work with them.
--
-- Enum is a bit tricky to work with, though.  Enum instances may get in the way of list fusion <https://gitlab.haskell.org/ghc/ghc/-/issues/18178> so underlying integer types need to be used. Newtype wrappers around Storable versions of these types and isos between them are provided, suffixed by S for Storable.
--
-- The tricky bit is making the isos zero-cost.
--
-- https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/
--
-- https://www.fpcomplete.com/haskell/tutorial/profiling/
--
-- > from . to == id
-- > to . from == id
data Iso a b = Iso {from :: a -> b, to :: b -> a}

-- | wrapped Word8 representation of Rank
--
-- >>> to rankS $ RankS 0
-- Two
-- >>> to rankS $ RankS 12
-- Ace
newtype RankS = RankS {unrankS :: Word8} deriving (Eq, Show, Ord)

-- | isomorphism between Rank and Word8 reps
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

-- | Storable vector of ranks
newtype RanksS = RanksS {unranksS :: S.Vector Word8} deriving (Eq, Show, Ord)

-- | wrapped Word8 representation of Suit
--
-- >>> to suitS $ SuitS 0
-- Club
-- >>> to suitS $ SuitS 3
-- Spade
newtype SuitS = SuitS {unsuitS :: Word8} deriving (Eq, Show, Ord)

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

{-
instance Enum Card where
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
  toEnum x = let (d, m) = x `divMod` 4 in Card (toEnum d) (toEnum m)

instance Bounded Card where
  minBound = Card Two Heart
  maxBound = Card Ace Spade

instance Ord Card where
  (<=) c c' = rank c <= rank c'

-}

-- | Set of ranks in a hand
--
-- >>> ranks cs
-- fromList [Four,Five,Six,Seven,Nine,Ten,Ace]
ranks :: [Card] -> Set.Set Rank
ranks cs = Set.fromList $ rank <$> cs

-- | Set of suits in a hand
--
-- >>> suits cs
-- fromList [Club,Heart,Spade]
suits :: [Card] -> Set.Set Suit
suits cs = Set.fromList $ suit <$> cs

-- | wrapped Word8 representation of a Card
--
-- >>> to cardS $ CardS 0
-- Card {rank = Two, suit = Club}
-- >>> to cardS $ CardS 51
-- Card {rank = Ace, suit = Spade}
newtype CardS = CardS {uncardS :: Word8} deriving (Eq, Show, Ord)

-- | card type conversion
--
-- TODO: CardS speed test:
--
-- 1. coercion
--
-- > toCard (CardS x) = (\(r,s) -> Card (unsafeCoerce r) (unsafeCoerce s)) (x `divMod` 4)
-- > fromCard (Card r s) = CardS $ unsafeCoerce r * 4 + unsafeCoerce s
--
-- 2. divmod
-- >  fromCard c = fromEnum (rank c) * 4 + fromEnum (suit c)
-- >  toCard x = let (d, m) = x `divMod` 4 in Card (toEnum d) (toEnum m)
--
-- 3. Word8 + Word8 = Word16
cardS :: Iso Card CardS
cardS = Iso fromCard toCard

toCard :: CardS -> Card
toCard (CardS x) = let (r, s) = x `divMod` 4 in Card (toRank (RankS r)) (toSuit (SuitS s))

fromCard :: Card -> CardS
fromCard (Card r s) = CardS $ unrankS (fromRank r) * 4 + unsuitS (fromSuit s)

instance Pretty CardS where
  pretty = pretty . to cardS

-- | a storable vector of Word8s representing a vector of CardSs.
newtype CardsS = CardsS {uncardsS :: S.Vector Word8} deriving (Eq, Show, Ord)

instance Pretty CardsS where
  pretty = pretty . to cardsS

-- | a standard 52 card deck
allCardsS :: CardsS
allCardsS = CardsS $ S.fromList [0 .. 51]

-- | Extract rank.
--
-- >>> pretty $ toRankS (CardS 0)
-- 2
toRankS :: CardS -> RankS
toRankS (CardS c) = RankS $ c `div` 4

-- | Extract suit.
--
-- >>> pretty $ toSuitS (CardS 0)
-- c
toSuitS :: CardS -> SuitS
toSuitS (CardS c) = SuitS $ c `mod` 4

-- | Conversion between traversable card types
cardsS :: Iso [Card] CardsS
cardsS =
  Iso
    (CardsS . S.fromList . fmap (uncardS . fromCard))
    (fmap (toCard . CardS) . S.toList . uncardsS)

-- | Set of ranks in a hand
--
-- >>> applyV ranksSet (card7sS 2)
-- [RanksS {unranksS = [12,8,5,4,3]},RanksS {unranksS = [11,8,5,4,2,1]}]
ranksSet :: CardsS -> RanksS
ranksSet (CardsS xs) =
  RanksS $
    S.fromList $
      Set.toDescList $
        Set.fromList $
          S.toList $
            S.map (unrankS . toRankS . CardS) xs

-- | vector of ranks in a hand
toRanksS :: CardsS -> RanksS
toRanksS cs = RanksS $ S.map (`div` 4) $ uncardsS cs

-- | A flat storable vector of Word8s representing n 7-card sets.
newtype Cards2S = Cards2S {uncards2S :: S.Vector Word8} deriving (Eq, Show, Ord)

instance Semigroup Cards2S where
  (<>) (Cards2S x) (Cards2S x') = Cards2S (x <> x')

instance Monoid Cards2S where
  mempty = Cards2S S.empty

-- | Iso between a flat storable vector of Word8s and a boxed vector of storable word8s represnting cards.
--
-- >>> (\x -> (to cardsS7V $ from cardsS7V x) == x) (card7sS 1000)
-- True
cardsS7V :: Iso Cards2S (V.Vector CardsS)
cardsS7V = Iso (applyFlat 7 CardsS . uncards2S) (Cards2S . fold . V.map uncardsS)

-- | Iso between a list of lists of cards and a flat storable vector of Word8s
--
-- >>> (\x -> (to cardsS7L $ from cardsS7L x) == x) (card7sS 1000)
-- True
cardsS7L :: Iso Cards2S [[Card]]
cardsS7L =
  Iso
    (V.toList . applyFlat 7 (fmap (toEnum . fromEnum) . S.toList) . uncards2S)
    (Cards2S . S.fromList . fmap (toEnum . fromEnum) . mconcat)

-- | Apply a function that takes a vector by slicing the supplied main vector n times.
--
-- >>> V.toList $ applyFlat 7 (pretty . CardsS) (uncards2S $ card7sS 2)
-- [[Ac, 7s, Tc, 5s, 6d, 7c, 6s],[7s, 4s, Td, 3d, 6c, Kh, Ts]]
applyFlat :: (Storable s) => Int -> (S.Vector s -> a) -> S.Vector s -> V.Vector a
applyFlat k f s = V.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | Performance testing suggests that [[Card]] structures are fastest as flat Storable Vectors.
--
-- Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable for each slice.
applyFlatS :: (Storable s, Storable a) => Int -> (S.Vector s -> a) -> S.Vector s -> S.Vector a
applyFlatS k f s = S.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | Performance testing suggests that [[Card]] structures are fastest as flat Storable Vectors.
--
-- Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable Vector of the same shape for each slice.
applyFlatM :: (Storable s, Storable a) => Int -> (S.Vector s -> S.Vector a) -> S.Vector s -> S.Vector a
applyFlatM k f s = fold $ V.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | apply a cards function to a cards vector.
applyV :: (CardsS -> a) -> Cards2S -> V.Vector a
applyV f (Cards2S s) = applyFlat 7 (f . CardsS) s

-- | apply a cards function to a cards vector.
applyS :: (Storable a) => (CardsS -> a) -> Cards2S -> S.Vector a
applyS f (Cards2S s) = applyFlatS 7 (f . CardsS) s

-- | apply a cards vector function returning a storable vector to a cards2S vector.
applyM :: (Storable a) => (CardsS -> S.Vector a) -> Cards2S -> S.Vector a
applyM f s = applyFlatM 7 (f . CardsS) (uncards2S s)
