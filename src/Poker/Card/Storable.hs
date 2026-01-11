-- | Storable version of 'Poker.Card' and related types.
--
-- In general, this module provides:
--
-- - 'Storable' 'Vector' versions of types in Poker.Card, suffixed withan 'S'
--
-- - an 'Iso' conversion, named after the type and suffixed with an I.
module Poker.Card.Storable
  ( -- * Usage
    -- $usage

    -- * Card
    RankS (..),
    rankI,
    RanksS (..),
    ranksI,
    SuitS (..),
    suitI,
    SuitsS (..),
    suitsI,
    CardS (..),
    cardI,
    cardRankS,
    cardSuitS,
    CardsS (..),
    cardsI,
    allCardsS,
    cardRanksS,
    cardRanksSWithDups,
    Cards2S (..),
    cards2I,
    cardsS7V,

    -- * Vector application
    applyFlat,
    applyFlatV,
    apply,
    applyV,
  )
where

import Control.Category ((>>>))
import Control.DeepSeq
import Data.Foldable
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector.Storable (Storable)
import Data.Vector.Storable qualified as S
import Data.Word
import Optics.Core
import Poker.Card
import Prettyprinter (Pretty (pretty))
import Prelude

-- $setup
--
-- >>> import Poker.Card
-- >>> import Poker.Card.Storable
-- >>> import Optics.Core
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> cs' = [Card Ace Hearts,Card Seven Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades]
-- >>> cs = review cardsI cs'
-- >>> cs
-- CardsS {unwrapCardsS = [50,23,34,15,16,22,19]}
-- >>> pretty cs
-- Ah7sTh5s6c7h6s
--
-- >>> let cs2' = [Card Ten Clubs, Card Five Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades]
-- >>> let css' = [cs', cs2']
--
-- >>> css = review cards2I css'
-- >>> css
-- Cards2S {unwrapCards2S = [50,23,34,15,16,22,19,32,15,34,15,16,22,19]}

-- | Storable representation of 'Rank'
--
-- >>> view rankI $ RankS 0
-- Two
-- >>> view rankI $ RankS 12
-- Ace
--
-- >>> pretty $ RankS 12
-- A
newtype RankS = RankS {unwrapRankS :: Word8} deriving (Eq, Show, Ord, NFData)

instance Pretty RankS where
  pretty = pretty . view rankI

-- | isomorphism between 'Poker.Rank' and 'Poker.Card.Storable.RankS'
rankI :: Iso' RankS Rank
rankI = iso toRank fromRank

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
toRank _ = error "bad Rank value"

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

-- | Storable representation of 'Rank'
--
-- >>> pretty $ RanksS (S.fromList [0..12])
-- 23456789TJQKA
newtype RanksS = RanksS {unwrapRanksS :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

instance Pretty RanksS where
  pretty rs = rs & unwrapRanksS & S.toList & fmap (RankS >>> view rankI >>> pretty) & mconcat

-- | isomorphism between 'RanksS' and [Rank]
ranksI :: Iso' RanksS [Rank]
ranksI =
  iso
    (\x -> x & unwrapRanksS & S.toList & fmap (RankS >>> view rankI))
    (\xs -> xs & fmap (review rankI >>> unwrapRankS) & S.fromList & RanksS)

-- | Storable representation of 'Suit'
--
-- >>> view suitI $ SuitS 0
-- Clubs
-- >>> view suitI $ SuitS 3
-- Spades
--
-- >>> pretty $ SuitS 0
-- c
newtype SuitS = SuitS {unwrapSuitS :: Word8} deriving (Eq, Show, Ord, NFData)

instance Pretty SuitS where
  pretty = pretty . view suitI

-- | isomorphism between 'Suit' and 'SuitS'
suitI :: Iso' SuitS Suit
suitI = iso toSuit fromSuit

toSuit :: SuitS -> Suit
toSuit (SuitS 0) = Clubs
toSuit (SuitS 1) = Diamonds
toSuit (SuitS 2) = Hearts
toSuit (SuitS 3) = Spades
toSuit _ = error "bad Suit value"

fromSuit :: Suit -> SuitS
fromSuit Clubs = SuitS 0
fromSuit Diamonds = SuitS 1
fromSuit Hearts = SuitS 2
fromSuit Spades = SuitS 3

-- | Storable representation of 'Suit'
--
-- >>> pretty $ SuitsS (S.fromList [0..3])
-- cdhs
newtype SuitsS = SuitsS {unwrapSuitsS :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

instance Pretty SuitsS where
  pretty rs = rs & unwrapSuitsS & S.toList & fmap (SuitS >>> view suitI >>> pretty) & mconcat

-- | isomorphism between 'SuitsS' and [Suit]
suitsI :: Iso' SuitsS [Suit]
suitsI =
  iso
    (\x -> x & unwrapSuitsS & S.toList & fmap (SuitS >>> view suitI))
    (\xs -> xs & fmap (review suitI >>> unwrapSuitS) & S.fromList & SuitsS)

-- | Storable representation of a 'Card'
--
-- >>> view cardI $ CardS 0
-- Card {rank = Two, suit = Clubs}
-- >>> view cardI $ CardS 51
-- Card {rank = Ace, suit = Spades}
-- >>> pretty $ CardS 51
-- As
newtype CardS = CardS {unwrapCardS :: Word8} deriving (Eq, Show, Ord)

instance Pretty CardS where
  pretty = pretty . view cardI

-- | card conversion
cardI :: Iso' CardS Card
cardI = iso toCard fromCard

toCard :: CardS -> Card
toCard (CardS x) = let (r, s) = x `divMod` 4 in Card (toRank (RankS r)) (toSuit (SuitS s))

fromCard :: Card -> CardS
fromCard (Card r s) = CardS $ unwrapRankS (fromRank r) * 4 + unwrapSuitS (fromSuit s)

-- | Storable representation of ['Card'].
--
-- >>> pretty $ view cardsI cs
-- [Ah, 7s, Th, 5s, 6c, 7h, 6s]
newtype CardsS = CardsS {unwrapCardsS :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

instance Pretty CardsS where
  pretty cs = cs & view cardsI & fmap pretty & mconcat

-- | a standard 52 card deck (in ascending order).
--
-- >>> pretty allCardsS
-- 2c2d2h2s3c3d3h3s4c4d4h4s5c5d5h5s6c6d6h6s7c7d7h7s8c8d8h8s9c9d9h9sTcTdThTsJcJdJhJsQcQdQhQsKcKdKhKsAcAdAhAs
allCardsS :: CardsS
allCardsS = CardsS $ S.fromList [0 .. 51]

-- | Conversion between traversable card types
cardsI :: Iso' CardsS [Card]
cardsI =
  iso
    (fmap (view cardI . CardS) . S.toList . unwrapCardsS)
    (CardsS . S.fromList . fmap (unwrapCardS . review cardI))

-- | Extract rank from a CardS.
--
-- >>> Card Two Spades & review cardI & cardRankS & view rankI
-- Two
cardRankS :: CardS -> RankS
cardRankS (CardS c) = RankS $ c `div` 4

-- | Extract suit from a CardS.
--
-- >>> Card Two Spades & review cardI & cardSuitS & view suitI
-- Spades
cardSuitS :: CardS -> SuitS
cardSuitS (CardS c) = SuitS $ c `mod` 4

-- | Ranks in a hand (CardsS) (with no duplicates)
--
-- >>> pretty $ cardRanksS cs
-- AT765
cardRanksS :: CardsS -> RanksS
cardRanksS (CardsS xs) =
  RanksS $
    S.fromList $
      Set.toDescList $
        Set.fromList $
          S.toList $
            S.map (unwrapRankS . cardRankS . CardS) xs

-- | RanksS of a hand (CardsS) (with duplicates)
--
-- >>> pretty $ cardRanksSWithDups cs
-- A7T5676
cardRanksSWithDups :: CardsS -> RanksS
cardRanksSWithDups cs = RanksS $ S.map (unwrapRankS . cardRankS . CardS) $ unwrapCardsS cs

-- | Storable representation of a [['Card']], where the length of each inner list is 7.
--
-- >>> pretty $ view cards2I css
-- [[Ah, 7s, Th, 5s, 6c, 7h, 6s], [Tc, 5s, Th, 5s, 6c, 7h, 6s]]
newtype Cards2S = Cards2S {unwrapCards2S :: S.Vector Word8} deriving (Eq, Show, Ord, NFData)

instance Semigroup Cards2S where
  (<>) (Cards2S x) (Cards2S x') = Cards2S (x <> x')

instance Monoid Cards2S where
  mempty = Cards2S S.empty

-- | Convert between a list of 7 card lists and a 'Cards2SS'
cards2I :: Iso' Cards2S [[Card]]
cards2I =
  iso
    (fmap (fmap (toCard . CardS)) . V.toList . applyFlatV 7 S.toList . unwrapCards2S)
    (Cards2S . S.fromList . fmap (unwrapCardS . fromCard) . mconcat)

-- | Convert between a 'Cards2S' and a boxed vector of 'CardsS'.
--
-- The main purpose of this representation is to access vector operations for things that aren't storable.
cardsS7V :: Iso' Cards2S (V.Vector CardsS)
cardsS7V = iso (applyFlatV 7 CardsS . unwrapCards2S) (Cards2S . fold . V.map unwrapCardsS)

-- | Apply a function that takes a vector by slicing the supplied main vector n times.
applyFlatV :: (Storable s) => Int -> (S.Vector s -> a) -> S.Vector s -> V.Vector a
applyFlatV k f s = V.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = S.length s `div` k

-- | Apply a function that takes a vector by slicing the supplied main vector n times, and providing a Storable for each slice.
applyFlat :: (Storable s, Storable a) => Int -> (S.Vector s -> a) -> S.Vector s -> S.Vector a
applyFlat k f s = S.generate n (\i -> f (S.slice (k * i) k s))
  where
    n = S.length s `div` k

-- | apply a function to a cards vector, returning a boxed vector of the results.
applyV :: (CardsS -> a) -> Cards2S -> V.Vector a
applyV f (Cards2S s) = applyFlatV 7 (f . CardsS) s
{-# INLINE applyV #-}

-- | apply a function to a cards vector, returning a storable vector of the results.
apply :: (Storable a) => (CardsS -> a) -> Cards2S -> S.Vector a
apply f (Cards2S s) = applyFlat 7 (f . CardsS) s
