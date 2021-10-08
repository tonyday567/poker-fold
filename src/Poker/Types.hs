{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Poker API.
module Poker.Types
  ( -- * basic card types
    RankS (..),
    rankS,
    RanksS (..),
    SuitS (..),
    suitS,
    ranks,
    suits,
    CardS(..),
    cardS,
    allCardsS,
    toRankS,
    toSuitS,
    CardsS(..),
    cardsS,
    ranksSet,
    toRanksS,
    Cards2S(..),
    cardsS7V,
    cardsS7L,
    applyFlat,
    applyFlatS,
    applyFlatM,
    applyV,
    applyS,
    applyM,

    -- * hands
    ShapedHandS (..),
    shapedHandS,
    fromShapedHand,
    fromHand,
    toHands,
    toRepHand,
    RangedHand (..),
    stratText,
    enumBs,
    handTypeCount,
    any2,

    -- * tables
    TableCards (..),
    deal,
    SeatState (..),
    Table (..),
    numSeats,
    TableConfig (..),
    defaultTableConfig,
    makeTable,
    makeTableS,
    liveSeats,
    openSeats,
    nextHero,
    closed,
    liveHands,

    -- * Betting
    RawAction (..),
    actOn,
    always,
    allin,
    bet,
    apply,

    -- * Shuffling
    enum2,
    ishuffle,

    -- * infrastructure
    Iso(..),

    -- * lpad
    lpad,
  ) where

import Poker hiding (fromList)
import Data.Distributive (Distributive (..))
import Data.FormatN
import Data.Functor.Rep
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Read
import Lens.Micro
import NumHask.Array.Fixed as A hiding (apply)
import Prelude
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import Data.Word
import Data.Text (Text, pack)
import Data.Vector.Storable (Storable)
import Prettyprinter hiding (comma)
import Prettyprinter.Render.Text (renderStrict)
import GHC.Generics
import Data.Bool
import Data.Foldable
import GHC.Exts hiding (toList)
import Data.Maybe

-- $setup
--
-- >>> import Poker
-- >>> import Poker.Types
-- >>> import Poker.Random
-- >>> import Prettyprinter
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.List as List
-- >>> import qualified Data.Vector.Storable as S
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Text as Text
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
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
-- > liso . riso == id
-- > riso . liso == id
--
data Iso a b = Iso { liso :: a -> b, riso :: b -> a }

-- | wrapped Word8 representation of Rank
--
-- >>> riso rankS $ RankS 0
-- Two
-- >>> riso rankS $ RankS 12
-- Ace
newtype RankS = RankS { unrankS :: Word8 } deriving (Eq, Show, Ord)

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

instance Pretty RankS
  where
    pretty = pretty . riso rankS

-- | Storable vector of ranks
newtype RanksS = RanksS { unranksS :: S.Vector Word8 } deriving (Eq, Show, Ord)

-- | wrapped Word8 representation of Suit
--
-- >>> riso suitS $ SuitS 0
-- Club
-- >>> riso suitS $ SuitS 3
-- Spade
--
newtype SuitS = SuitS { unsuitS :: Word8 } deriving (Eq, Show, Ord)

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
  pretty = pretty . riso suitS

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
-- >>> riso cardS $ CardS 0
-- Card {rank = Two, suit = Club}
-- >>> riso cardS $ CardS 51
-- Card {rank = Ace, suit = Spade}
--
newtype CardS = CardS { uncardS :: Word8 } deriving (Eq, Show, Ord)

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
toCard (CardS x) = let (r,s) = x `divMod` 4 in Card (toRank (RankS r)) (toSuit (SuitS s))

fromCard :: Card -> CardS
fromCard (Card r s) = CardS $ unrankS (fromRank r) * 4 + unsuitS (fromSuit s)

instance Pretty CardS
  where
    pretty = pretty . riso cardS

-- | a storable vector of Word8s representing a vector of CardSs.
--
newtype CardsS = CardsS { uncardsS :: S.Vector Word8 } deriving (Eq, Show, Ord)

instance Pretty CardsS
  where
    pretty = pretty . riso cardsS

-- | a standard 52 card deck
--
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
newtype Cards2S = Cards2S { uncards2S :: S.Vector Word8 } deriving (Eq, Show, Ord)

instance Semigroup Cards2S where
  (<>) (Cards2S x) (Cards2S x') = Cards2S (x <> x')

instance Monoid Cards2S where
  mempty = Cards2S S.empty

-- | Iso between a flat storable vector of Word8s and a boxed vector of storable word8s represnting cards.
--
-- >>> (\x -> (riso cardsS7V $ liso cardsS7V x) == x) (card7sS 1000)
-- True
cardsS7V :: Iso Cards2S (V.Vector CardsS)
cardsS7V = Iso (applyFlat 7 CardsS . uncards2S) (Cards2S . fold . V.map uncardsS)

-- | Iso between a list of lists of cards and a flat storable vector of Word8s
--
-- >>> (\x -> (riso cardsS7L $ liso cardsS7L x) == x) (card7sS 1000)
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
applyFlat k f s = V.generate n (\i -> f (S.slice (k*i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | Performance testing suggests that [[Card]] structures are fastest as flat Storable Vectors.
--
-- Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable for each slice.
applyFlatS :: (Storable s, Storable a) => Int -> (S.Vector s -> a) -> S.Vector s -> S.Vector a
applyFlatS k f s = S.generate n (\i -> f (S.slice (k*i) k s))
  where
    n = fromIntegral $ S.length s `div` k

-- | Performance testing suggests that [[Card]] structures are fastest as flat Storable Vectors.
--
-- Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable Vector of the same shape for each slice.
applyFlatM :: (Storable s, Storable a) => Int -> (S.Vector s -> S.Vector a) -> S.Vector s -> S.Vector a
applyFlatM k f s = fold $ V.generate n (\i -> f (S.slice (k*i) k s))
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

-- | Card pairs dealt to a holdem player have identical probability structures to each other, when viewed through a stochastic future of betting action.
--
-- These groupings or iso-sets of card pairs map to a dense representation of what a hand should do, given an element of this iso-set is in their hands. This representation forms a basis for modelling player actions, in the presence of uncertainty about what other seats hold, and what they will do about it.
--
-- A (Card, Card) can be:
--
-- - a pair (the same rank): in 12 enumerations of a deck. (4 suits x 3 suits)
--
-- - offsuited (of different rank): in 24 enumerations of a deck: (low | high ranked) x 4 suits x 3 suits
--
-- - suited (of different rank and the same suit): in 8 enumerations of a deck ((low | high ranked) x 4 suits)
--
-- ![count example](other/count.svg)
--
-- The transformation from (Card, Card) to Hand reduces the strategy vector size from 52*51 to 169, and provides expansion back into the (Card, Card) domain when needed.
--
--
newtype ShapedHandS = ShapedHandS { unShapedHandS :: Word8} deriving (Eq, Show, Ord)

shapedHandS :: Iso ShapedHand ShapedHandS
shapedHandS =
  Iso fromShapedHand toShapedHand

fromShapedHand :: ShapedHand -> ShapedHandS
fromShapedHand (MkOffsuit r0 r1) =
  ShapedHandS $ unrankS (fromRank r1) * 13 + unrankS (fromRank r0)
fromShapedHand (MkPair p) =
  ShapedHandS $ let p' = unrankS (fromRank p) in p' * 13 + p'
fromShapedHand (MkSuited r0 r1) =
  ShapedHandS $ unrankS (fromRank r0) * 13 + unrankS (fromRank r1)

toShapedHand :: ShapedHandS -> ShapedHand
toShapedHand (ShapedHandS x) = case compare d m of
    EQ -> MkPair $ toRank . RankS $ d
    LT -> MkOffsuit (toRank . RankS $ m) (toRank . RankS $ d)
    GT -> MkSuited (toRank . RankS $ d) (toRank . RankS $ m)
    where
      (d, m) = x `divMod` 13

-- | convert from a Card pair to a Hand
--
-- The base mechanism of this transformation is to forget suit details.
--
-- >>> fromHand (MkHand (Card Ace Heart) (Card Ace Spade))
-- MkPair Ace
--
-- Unpaired cards are forced to high low order.
--
-- >>> fromHand (MkHand (Card Two Heart) (Card Ace Spade))
-- MkOffsuit Ace Two
fromHand :: Hand -> ShapedHand
fromHand (Hand (Card r s) (Card r' s'))
  | r == r' = MkPair r
  | s == s' = MkSuited (max r r') (min r r')
  | otherwise = MkOffsuit (max r r') (min r r')

-- | Enumeration of the Hands that a ShapedHand represents.
--
-- >>> pretty $ toHands (MkPair Ace)
-- [AcAd, AcAh, AcAs, AdAc, AdAh, AdAs, AhAc, AhAd, AhAs, AsAc, AsAd, AsAh]
toHands :: ShapedHand -> [Hand]
toHands (MkPair r) = (\(x,y) -> MkHand (Card r x) (Card r y)) <$> enum2 allSuits
toHands (MkSuited r0 r1) =
  ((\x -> MkHand (Card r0 x) (Card r1 x)) <$> allSuits) <>
  ((\x -> MkHand (Card r1 x ) (Card r0 x)) <$> allSuits)
toHands (MkOffsuit r0 r1) =
  ((\(x,y) -> MkHand (Card r0 x) (Card r1 y)) <$> enum2 allSuits) <>
  ((\(x,y) -> MkHand (Card r0 y) (Card r1 x)) <$> enum2 allSuits)

-- | a representative pair of cards for a B, choosing Club and Diamond.
--
-- Always have a good think about this in the realm of raw card simulation.
toRepHand :: ShapedHand -> Hand
toRepHand (MkPair r) = MkHand (Card r Club) (Card r Diamond)
toRepHand (MkSuited r0 r1) = MkHand (Card r0 Club) (Card r1 Club)
toRepHand (MkOffsuit r0 r1) = MkHand (Card r0 Club) (Card r1 Diamond)

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

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
ishuffle :: [Int] -> [Int]
ishuffle as = reverse $ go as []
  where
    go [] s = s
    go (x0 : xs) s = go xs (x1:s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + 1) (d <= acc)) x0 s

-- | The spirit of RangedHand is to be a representable functor of a's indexed by ShapedHand.
--
-- A RangedHand can be many things:
--
-- One example is as a statistic across the set of hands. Here is a chart of the chances of winning given a Hand, against another player with any2.
--
-- ![bwin example](other/bwin.svg)
--
-- Another example is as a strategy for a seat: what betting action should be taken, given what I might be holding in my Hand.
--
-- >>> :t always RawCall
-- always RawCall :: RangedHand RawAction
--
-- Or the dual to this question: given the betting action that has occurred, what are my guesses about their Hand. (FIXME: NYI)
newtype RangedHand a = RangedHand
  { array :: Array '[169] a
  }
  deriving (Eq, Foldable)

instance (Show a) => Show (RangedHand a) where
  show (RangedHand a) = show (toList a)

instance (Read a) => Read (RangedHand a) where
  readPrec = RangedHand . fromList <$> readPrec

instance Functor RangedHand where
  fmap f (RangedHand a) = RangedHand (fmap f a)

instance Data.Distributive.Distributive RangedHand where
  distribute = distributeRep

instance Representable RangedHand where
  type Rep RangedHand = ShapedHandS

  tabulate f = RangedHand $ tabulate (f . ShapedHandS . toEnum . fromMaybe 0 . listToMaybe)

  index (RangedHand a) = index a . (: []) . fromEnum . unShapedHandS

instance Pretty (RangedHand Text) where
  pretty s =
    pretty $
    Text.intercalate
      "\n"
      ( ( \x ->
            Text.intercalate
              " "
              --
              ( (\y -> index s (ShapedHandS $ 13 * (12 - y) + (12 - x)))
                  <$> [0 .. 12]
              )
        )
          <$> [0 .. 12]
      )

instance Pretty (RangedHand Char) where
  pretty s =
    pretty $
    Text.intercalate
      "\n"
      ( ( \x ->
            Text.intercalate
              ""
              ( (\y -> Text.singleton $ index s (ShapedHandS $ 13 * (12 - x) + (12 - y)))
                  <$> [0 .. 12]
              )
        )
          <$> [0 .. 12]
      )

instance Pretty (RangedHand RawAction) where
  pretty s = pretty $ stratSym ("f", "c", "r", "r") 10 s

-- | screen representation of a RangedHand
--
-- >>> pretty stratText
-- AAp AKo AQo AJo ATo A9o A8o A7o A6o A5o A4o A3o A2o
-- AKs KKp KQo KJo KTo K9o K8o K7o K6o K5o K4o K3o K2o
-- AQs KQs QQp QJo QTo Q9o Q8o Q7o Q6o Q5o Q4o Q3o Q2o
-- AJs KJs QJs JJp JTo J9o J8o J7o J6o J5o J4o J3o J2o
-- ATs KTs QTs JTs TTp T9o T8o T7o T6o T5o T4o T3o T2o
-- A9s K9s Q9s J9s T9s 99p 98o 97o 96o 95o 94o 93o 92o
-- A8s K8s Q8s J8s T8s 98s 88p 87o 86o 85o 84o 83o 82o
-- A7s K7s Q7s J7s T7s 97s 87s 77p 76o 75o 74o 73o 72o
-- A6s K6s Q6s J6s T6s 96s 86s 76s 66p 65o 64o 63o 62o
-- A5s K5s Q5s J5s T5s 95s 85s 75s 65s 55p 54o 53o 52o
-- A4s K4s Q4s J4s T4s 94s 84s 74s 64s 54s 44p 43o 42o
-- A3s K3s Q3s J3s T3s 93s 83s 73s 63s 53s 43s 33p 32o
-- A2s K2s Q2s J2s T2s 92s 82s 72s 62s 52s 42s 32s 22p
stratText :: RangedHand Text
stratText = renderStrict . layoutCompact . pretty <$> tabulate toShapedHand

stratSym :: (Text, Text, Text, Text) -> Double -> RangedHand RawAction -> Text
stratSym (f, c, r, rr) b s =
  Text.intercalate
    "\n"
    ( ( \x ->
          Text.intercalate
            ""
            ( (\y -> toSym $ index s (ShapedHandS $ 13 * x + y))
                <$> [0 .. 12]
            )
      )
        <$> [0 .. 12]
    )
  where
    toSym RawFold = f
    toSym RawCall = c
    toSym (RawRaise x) = bool r rr (x > b)

-- | left pad some text
lpad :: Int -> Text -> Text
lpad n t = pack (replicate (n - Text.length t) ' ') <> t

-- | enumerate (Card, Card) and count the Bs
enumBs :: RangedHand Int
enumBs =
  tabulate
  (Map.fromListWith (+)
   ((,1) . liso shapedHandS . fromHand . uncurry MkHand <$>
    enum2 allCards)
    Map.!)

-- | The theoretical combinatorial count.
--
-- > fromIntegral <$> enumBs == handTypeCount
--
-- >>> import Prettyprinter.Render.Text (renderStrict)
-- >>> pretty $ (lpad 4 . renderStrict . layoutCompact . pretty) <$> handTypeCount
-- 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0 24.0
--  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 12.0
handTypeCount :: RangedHand Double
handTypeCount = tabulate $ \x -> case toShapedHand x of
  (Pair _) -> 12
  (Suited _ _) -> 8
  (Offsuit _ _) -> 24

-- | A RangedHand with no information about the Hand.
any2 :: RangedHand Double
any2 = (/ sum handTypeCount) <$> handTypeCount

-- | A typical poker table setup for texas holdem.
--
-- - each player gets 2 cards. There are typically 2 to 9 players.
--
-- - there are 5 hole cards
--
-- >>> pretty $ deal cs
-- Ah7s Th5s|6c7h6s|9h|4s
data TableCards = TableCards
  { playerCards :: [Hand],
    flopCards :: (Card, Card, Card),
    turnCard :: Card,
    riverCard :: Card
  }
  deriving (Eq, Show, Generic)

instance Pretty TableCards where
  pretty (TableCards ps (f0,f1,f2) t r) =
    concatWith (surround "|")
      [ hsep $ (\(Hand x y) -> pretty x <> pretty y) <$> toList ps,
        pretty f0 <> pretty f1 <> pretty f2,
        pretty t,
        pretty r
      ]

-- | Deal table cards
deal :: [Card] -> TableCards
deal cs =
  TableCards
  (fromList
   ((\x ->
      MkHand (cs List.!! (2 * x)) (cs List.!! (2 * x + 1))) <$>
     [0 .. n - 1]))
  (cs List.!! (n * 2), cs List.!! (1 + n * 2), cs List.!! (2 + n * 2))
  (cs List.!! (3 + n * 2))
  (cs List.!! (4 + n * 2))
  where
    n = (length cs - 5) `div` 2

-- | For each seat, the betting can be open (can re-raise), closed (has called and cannot re-raise). A raise at the table re-opens the betting for all live seats.
--
-- SittingOut would be an extra sum type of you would need in live poker.
data SeatState = BettingOpen | BettingClosed | Folded deriving (Eq, Show, Generic)

instance Pretty SeatState where
  pretty BettingOpen = "o"
  pretty BettingClosed = "c"
  pretty Folded = "f"

-- | Table state.
--
-- hero is poker jargon for a cursor into the next seat to act.
--
-- An alternative structure would be a Player type say, with card pair, Seat, stack, bet, but this seems artificial given likely computations that will be happening.
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,
data Table = Table
  { cards :: TableCards,
    hero :: Maybe Int,
    seats :: Seq.Seq SeatState,
    stacks :: Seq.Seq Double,
    bets :: Seq.Seq Double,
    pot :: Double,
    history :: Seq.Seq (RawAction, Int)
  }
  deriving (Eq, Show, Generic)

-- | number of seats at the table
--
-- >>> numSeats t
-- 2
numSeats :: Table -> Int
numSeats ts = length (ts ^. #seats)

instance Pretty Table where
  pretty (Table cs n s st bs p h) =
    concatWith (surround ",")
      [ pretty cs,
        "hero: " <> pretty n,
        hsep $ pretty <$> toList s,
        hsep $ pretty . comma (Just 2) <$> toList st,
        hsep $ pretty . comma (Just 2) <$> toList bs,
        pretty (comma (Just 2) p),
        concatWith (surround ":") $ (\(a, p) -> pretty a <> pretty p) <$> toList h
      ]

-- | list of active player indexes
--
-- >>> liveSeats t
-- [0,1]
liveSeats :: Table -> [Int]
liveSeats ts =
  fst
    <$> filter
      ((/= Folded) . snd)
      (zip [0 ..] (toList $ ts ^. #seats))

-- | list of non-hero actives who can still bet
--
-- >>> openSeats t
-- [1]
openSeats :: Table -> [Int]
openSeats ts = case ts ^. #hero of
  Nothing -> []
  Just h ->
    fst
      <$> filter
        ((== BettingOpen) . snd)
        (nexts $ zip [0 ..] (toList $ ts ^. #seats))
    where
      nexts l = drop (h + 1) l <> take h l

-- | next seat open to bet
--
-- >>> nextHero t
-- Just 1
nextHero :: Table -> Maybe Int
nextHero ts = listToMaybe (openSeats ts)

-- | The table is closed when no seat is open, or all but 1 seat has folded.
--
-- >>> closed t
-- False
closed :: Table -> Bool
closed ts =
  notElem BettingOpen (ts ^. #seats)
    || length (filter (/= Folded) (toList $ ts ^. #seats)) <= 1

-- | Index of seat and hands in the pot
--
-- >>> pretty $ liveHands t
-- [(0, [Ah, 7s, 6c, 7h, 6s, 9h, 4s]), (1, [Th, 5s, 6c, 7h, 6s, 9h, 4s])]
liveHands :: Table -> [(Int, [Card])]
liveHands ts = (\i -> hands (ts ^. #cards) List.!! i) <$> liveSeats ts

-- | Provide the player hands combined with the table cards.
--
hands :: TableCards -> [(Int, [Card])]
hands (TableCards ps (f0,f1,f2) t r) =
  zip
  [0 .. (length ps - 1)]
  ((\(Hand x y) -> [x, y, f0, f1, f2, t , r]) <$> ps)

-- | Static configuration for setting up a table.
--
-- >>> defaultTableConfig
-- TableConfig {numPlayers = 2, ante = 0.0, stacks0 = fromList [10.0,10.0]}
data TableConfig = TableConfig
  { numPlayers :: Int,
    ante :: Double,
    stacks0 :: Seq.Seq Double
  }
  deriving (Eq, Show, Generic)

-- | Default is 2 seats, no antes, and equal stacks of 10.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 2 0 (Seq.replicate 2 10)

-- | Construct a Table with the supplied cards.
makeTable :: TableConfig -> [Card] -> Table
makeTable cfg cs = Table (deal cs) (Just 0) (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0 Seq.Empty
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | Construct a Table with the supplied cards.
makeTableS :: TableConfig -> CardsS -> Table
makeTableS cfg cs = Table (deal (riso cardsS cs)) (Just 0) (Seq.replicate (cfg ^. #numPlayers) BettingOpen) (Seq.zipWith (-) (cfg ^. #stacks0) bs) bs 0 Seq.Empty
  where
    bs = bbs (cfg ^. #numPlayers) (cfg ^. #ante)

-- | standard blind and ante chip structure for n seats.
bbs :: Int -> Double -> Seq.Seq Double
bbs n ante = Seq.fromList $ reverse $ [1 + ante, 0.5 + ante] <> replicate (n - 2) ante

-- | There are three primitive actions that seats must perform when they are the hero:
--
-- Fold. A seat can only fold if there are bets out above their current bet, and they have some chips. The Seat becomes Folded and the hand is discarded.
--
-- Call. A seat can bet the difference between the maximum bet and their current bet, or the rest of their stack if this is less. This is called a Call and is the minimum allowed bet action. A check is when zero is the minimum.
--
-- Raise x. A seat bets x above the minimum allowed bet, where x <= stack - minimum allowed.
data RawAction = RawFold | RawCall | RawRaise Double deriving (Eq, Show, Generic)

instance Pretty RawAction where
  pretty RawFold = "f"
  pretty RawCall = "c"
  pretty (RawRaise x) = pretty $ fixed (Just 1) x <> "r"

-- | Always perform an action
always :: RawAction -> RangedHand RawAction
always a = tabulate (const a)

-- | Raise to the hero's stack size.
allin :: Table -> RangedHand RawAction
allin ts = tabulate (const (RawRaise x))
  where
    x = Seq.index (ts ^. #stacks) (fromMaybe 0 $ ts ^. #hero)

-- | A game progresses by players taking an action, which alters a table state.
--
-- >>> pretty t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- A 2 player table, where stacks start at 10 each, hero is seat 0, Big blind is seat 1. seat 1 posts the big blind, seat 0 posts the small blind. hero, as utg, is first action.
--
-- s0: Restricting the strategy action set to Fold, Call or Raise 10, seat 0 strategy (s0) branches into:
--
-- - s0: Fold
--
-- >>> pretty (actOn RawFold t)
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,f o,9.5 9,0 1,0.5,f0
--
-- >>> closed (actOn RawFold t)
-- True
--
-- - s0: Call
--
-- >>> pretty (actOn RawCall t)
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,c o,9 9,1 1,0,c0
--
-- s1: s1 is the strategy for seat 1, given betting history of [s0:Call]. They are open for betting (can actOn). They can Call or Raise 10
--
--     - s1: Call. At this point, we assume no further betting (this is equivalent to neither player having an advantage post-flop), and resolve the table.
--
-- >>> pretty $ actOn RawCall $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,9 9,1 1,0,c1:c0
--
-- Seat 0 wins a small pot.
--
--     - s1: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 0,o c,9 0,1 10,0,9.0r1:c0
--
-- (s2) is the strategy for seat 0, given betting history of [s0:Call, s1:Raise 10]
--       - s2: Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,f c,9 0,0 10,1,f0:9.0r1:c0
--
--       - s2: Call
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) $ actOn RawCall t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,0 0,10 10,0,c0:9.0r1:c0
--
-- Table is closed for betting (hero == Nothing), and the small blind wins a big pot with a pair of sevens after calling the big blinds allin.
--
-- - s0: Raise 10
--
-- >>> pretty $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: 1,c o,0 9,10 1,0,9.0r0
--
-- (s3) is the strategy for seat 1, given betting history of [s0:Raise 10]
--
--     - s3:Fold
--
-- >>> pretty $ actOn RawFold $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c f,0 9,10 0,1,f1:9.0r0
--
--     - s3:Call
--
-- >>> pretty $ actOn RawCall $ actOn (RawRaise 10) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
--
-- One of the reasons actOn is separated from apply is that it can change the incoming Action from a strategy, given table conditions. This may be a design flaw that can be ironed out.
actOn :: RawAction -> Table -> Table
actOn RawFold ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    -- order of execution matters
    ts
      & #bets %~ Seq.update p 0
      & #pot %~ (+ Seq.index (ts ^. #bets) p)
      & #seats
        %~ bool
          (Seq.update p BettingClosed)
          (Seq.update p Folded)
          -- last player cant fold
          (length (liveSeats ts) > 1)
      -- hero calculation needs to take into account updated seat status
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((bool RawCall RawFold (length (liveSeats ts) > 1), p) Seq.:<|)
actOn RawCall ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
      & #bets %~ Seq.adjust' (+ bet) p
      & #stacks %~ Seq.adjust' (\x -> x - bet) p
      & #seats %~ Seq.update p BettingClosed
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((RawCall, p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min gap st
actOn (RawRaise r) ts = case ts ^. #hero of
  Nothing -> ts
  Just p ->
    ts
      & #bets %~ Seq.adjust' (+ bet) p
      & #stacks %~ Seq.adjust' (\x -> x - bet) p
      & #seats %~ Seq.update p (bool BettingClosed BettingOpen (st' > 0))
      & ( \t ->
            t
              & bool
                id
                ( #seats
                    .~ Seq.zipWith
                      ( \x st'' ->
                          bool x BettingOpen (x == BettingClosed && st'' > 0)
                      )
                      (t ^. #seats)
                      (t ^. #stacks)
                )
                (r' > 0)
        )
      & (\t -> t & #hero .~ nextHero t)
      & #history %~ ((bool RawCall (RawRaise r') (r' > 0), p) Seq.:<|)
    where
      gap = maximum (ts ^. #bets) - Seq.index (ts ^. #bets) p
      st = Seq.index (ts ^. #stacks) p
      bet = min (gap + r) st
      r' = bet - gap
      st' = st - bet

-- | Follow a betting pattern according to a strategy list, until betting is closed, or the list ends.
--
-- >>> pretty $ bet (always (RawRaise 10) : (replicate 3 (always RawCall))) t
-- Ah7s Th5s|6c7h6s|9h|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
bet :: [RangedHand RawAction] -> Table -> Table
bet ss t = go ss t
  where
    go [] t = t
    go (s : ss') t =
      bool (bet ss' (actOn (apply s t) t)) t (closed t)

-- | Apply a strategy to a table, supplying the Action for the hero, if any.
--
-- >>> apply (always RawCall) t
-- RawCall
apply :: RangedHand RawAction -> Table -> RawAction
apply s t = fromMaybe RawFold $ case t ^. #hero of
  Nothing -> error "bad juju"
  Just i -> Just $ index s (liso shapedHandS $ fromHand ((t ^. #cards . #playerCards) List.!! i))

