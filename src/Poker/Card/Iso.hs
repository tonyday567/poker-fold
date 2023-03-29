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

-- | Evaluation of a standard holdem poker hand. The evaluators work for 5 and 7 card hands.
--
-- The module supplies 'handRank' and helpers for evaluation of a ['Card']
module Poker.Card.Iso
  ( -- * Usage
    -- $usage

    rankI,
    suitI,
    ranks,
    suits,
    cardI,
    cardsI,
    cards7I,
  )
where

import Control.Applicative
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import GHC.Word
import Optics.Core
import Poker.Card as C
import qualified Poker.Card.Storable as PCS
import Prelude

-- | isomorphism between 'Poker.Rank' and 'Poker.Card.Storable.Rank'
rankI :: Iso' PCS.Rank C.Rank
rankI = iso toRank fromRank

toRank :: PCS.Rank -> C.Rank
toRank (PCS.Rank 0) = Two
toRank (PCS.Rank 1) = Three
toRank (PCS.Rank 2) = Four
toRank (PCS.Rank 3) = Five
toRank (PCS.Rank 4) = Six
toRank (PCS.Rank 5) = Seven
toRank (PCS.Rank 6) = Eight
toRank (PCS.Rank 7) = Nine
toRank (PCS.Rank 8) = Ten
toRank (PCS.Rank 9) = Jack
toRank (PCS.Rank 10) = Queen
toRank (PCS.Rank 11) = King
toRank (PCS.Rank 12) = Ace
toRank _ = error "bad Rank value"

fromRank :: Rank -> PCS.Rank
fromRank Two = PCS.Rank 0
fromRank Three = PCS.Rank 1
fromRank Four = PCS.Rank 2
fromRank Five = PCS.Rank 3
fromRank Six = PCS.Rank 4
fromRank Seven = PCS.Rank 5
fromRank Eight = PCS.Rank 6
fromRank Nine = PCS.Rank 7
fromRank Ten = PCS.Rank 8
fromRank Jack = PCS.Rank 9
fromRank Queen = PCS.Rank 10
fromRank King = PCS.Rank 11
fromRank Ace = PCS.Rank 12

-- | Storable representation of 'Suit'
--
-- >>> to suit $ Suit 0
-- Club
-- >>> to suit $ Suit 3
-- Spade
newtype Suit = Suit {unwrapSuit :: Word8} deriving (Eq, Show, Ord)

-- | isomorphism between 'Suit' and 'SuitS'
suitI :: Iso' PCS.Suit C.Suit
suitI = iso toSuit fromSuit

toSuit :: PCS.Suit -> C.Suit
toSuit (PCS.Suit 0) = Club
toSuit (PCS.Suit 1) = Diamond
toSuit (PCS.Suit 2) = Heart
toSuit (PCS.Suit 3) = Spade
toSuit _ = error "bad Suit value"

fromSuit :: C.Suit -> PCS.Suit
fromSuit Club = PCS.Suit 0
fromSuit Diamond = PCS.Suit 1
fromSuit Heart = PCS.Suit 2
fromSuit Spade = PCS.Suit 3

-- | Storable representation of ['Suit']
--
-- >>> from suits allSuits
-- Suits {unwrapSuits = [0,1,2,3]}
--
-- >>> pretty $ from suits allSuits
-- cdhs
newtype Suits = Suits {unwrapSuits :: S.Vector Word8} deriving (Eq, Show, Ord)

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
suits :: [Card] -> Set.Set C.Suit
suits cs = Set.fromList $ suit <$> cs

-- | card conversion
cardI :: Iso' C.Card PCS.Card
cardI = iso fromCard toCard

toCard :: PCS.Card -> C.Card
toCard (PCS.Card x) = let (r, s) = x `divMod` 4 in Card (toRank (PCS.Rank r)) (toSuit (PCS.Suit s))

fromCard :: C.Card -> PCS.Card
fromCard (C.Card r s) = PCS.Card $ PCS.unwrapRank (fromRank r) * 4 + PCS.unwrapSuit (fromSuit s)

-- | Conversion between traversable card types
cardsI :: Iso' [C.Card] PCS.Cards
cardsI =
  iso
    (PCS.Cards . S.fromList . fmap (PCS.unwrapCard . view cardI))
    (fmap (review cardI . PCS.Card) . S.toList . PCS.unwrapCards)

-- | Convert between a list of 7 card lists and a 'Cards2S'
--
-- >>> (\x -> (to cardsS7L $ from cardsS7L x) == x) css
-- True
cards7I :: Iso' [[C.Card]] PCS.Cards2
cards7I =
  iso
    (PCS.Cards2 . S.fromList . fmap (PCS.unwrapCard . fromCard) . mconcat)
    (fmap (fmap (toCard . PCS.Card)) . V.toList . PCS.applyFlatV 7 S.toList . PCS.unwrapCards2)
