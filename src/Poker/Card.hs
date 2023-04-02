{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Poker.Card
  ( Rank (..),
    allRanks,
    Suit (..),
    allSuits,
    suitToUnicode,
    suitFromUnicode,
    Card (..),
    allCards,
    Hole (..),
    allHoles,
    rankToChr,
    chrToRank,
    suitToChr,
    chrToSuit,
    cardToShortTxt,
    cardFromShortTxt,
    ranks,
    suits,
  )
where

import Control.Monad (liftM2)
import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Internal
import qualified Data.Set as Set

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Poker.Card
-- >>> import Data.Maybe
-- >>> import Control.Monad.State.Lazy
-- >>> import Data.Bool
-- >>> import Data.Functor.Rep
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Text as Text
-- >>> let cs = [Card Ace Hearts,Card Seven Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades]
-- >>> :t cs
-- cs :: [Card]

-- | The 'Rank' of a playing 'Card'
data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Rank where
  pretty = unsafeTextWithoutNewlines . T.singleton . rankToChr

-- |
-- >>> allRanks
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
allRanks :: [Rank]
allRanks = [minBound .. maxBound]

-- |
-- >>> rankToChr <$> allRanks
-- "23456789TJQKA"
rankToChr :: Rank -> Char
rankToChr = \case
  Two -> '2'
  Three -> '3'
  Four -> '4'
  Five -> '5'
  Six -> '6'
  Seven -> '7'
  Eight -> '8'
  Nine -> '9'
  Ten -> 'T'
  Jack -> 'J'
  Queen -> 'Q'
  King -> 'K'
  Ace -> 'A'

-- |
-- >>> catMaybes $ chrToRank <$> "23456789TJQKA"
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
--
-- >>> chrToRank 'f'
-- Nothing
--
chrToRank :: Char -> Maybe Rank
chrToRank = \case
  '2' -> pure Two
  '3' -> pure Three
  '4' -> pure Four
  '5' -> pure Five
  '6' -> pure Six
  '7' -> pure Seven
  '8' -> pure Eight
  '9' -> pure Nine
  'T' -> pure Ten
  'J' -> pure Jack
  'Q' -> pure Queen
  'K' -> pure King
  'A' -> pure Ace
  _ -> Nothing

-- | The 'Suit' of a playing 'Card'
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Suit where
  pretty = Char . suitToChr

-- |
-- >>> allSuits
-- [Clubs,Diamonds,Hearts,Spades]
allSuits :: [Suit]
allSuits = [minBound .. maxBound]

-- |
-- >>> suitToChr <$> allSuits
-- "cdhs"
suitToChr :: Suit -> Char
suitToChr = \case
  Clubs -> 'c'
  Diamonds -> 'd'
  Hearts -> 'h'
  Spades -> 's'

-- |
-- >>> map (fromJust . chrToSuit) "cdhs"
-- [Clubs,Diamonds,Hearts,Spades]
-- >>> chrToSuit '1'
-- Nothing
--
chrToSuit :: Char -> Maybe Suit
chrToSuit = \case
  'c' -> pure Clubs
  'd' -> pure Diamonds
  'h' -> pure Hearts
  's' -> pure Spades
  _ -> Nothing

-- |
-- >>> suitToUnicode <$> [Clubs, Diamonds, Hearts, Spades]
-- "\9827\9830\9829\9824"
-- >>> fromJust . suitFromUnicode . suitToUnicode <$> [Clubs, Diamonds, Hearts, Spades]
-- [Clubs,Diamonds,Hearts,Spades]
suitToUnicode :: Suit -> Char
suitToUnicode = \case
  Clubs -> '♣'
  Diamonds -> '♦'
  Hearts -> '♥'
  Spades -> '♠'

-- |
-- >>> suitFromUnicode <$> ['♣', '♦', '♥', '♠']
-- [Just Clubs,Just Diamonds,Just Hearts,Just Spades]
suitFromUnicode :: Char -> Maybe Suit
suitFromUnicode = \case
  '♣' -> Just Clubs
  '♦' -> Just Diamonds
  '♥' -> Just Hearts
  '♠' -> Just Spades
  _ -> Nothing

-- | Representation of a playing card.
data Card = Card
  { rank :: !Rank,
    suit :: !Suit
  }
  deriving (Eq, Ord, Show, Read)

instance Pretty Card where
  pretty Card {rank = r, suit = s} = pretty r <> pretty s

instance IsString Card where
  fromString = fromJust . cardFromShortTxt . T.pack

-- | All cards in deck
allCards :: [Card]
allCards = liftM2 Card allRanks allSuits

cardToShortTxt :: Card -> Text
cardToShortTxt (Card r s) = T.pack [rankToChr r, suitToChr s]

cardFromShortTxt :: Text -> Maybe Card
cardFromShortTxt cs = case second T.uncons <$> T.uncons cs of
  Just (r, Just (s, T.null -> True)) -> Card <$> chrToRank r <*> chrToSuit s
  _ -> Nothing

-- | 'Hole' represents a player's hole cards in a game of Texas Hold\'Em
--
-- >>> pretty $ Hole (Card Ace Hearts) (Card King Spades)
-- AhKs
data Hole = Hole !Card !Card
  deriving (Eq, Ord, Show)


instance Pretty Hole where
  pretty (Hole c1 c2) = pretty c1 <> pretty c2

-- | All possible Hold'Em poker 'Hole's
--
allHoles :: [Hole]
allHoles = reverse $ do
  r1 <- [minBound .. maxBound]
  r2 <- enumFrom r1
  (s1, s2) <-
    if r1 == r2
      then [(s1, s2) | s1 <- [minBound .. maxBound] , s2 <- drop 1 (enumFrom s1)]
      else liftM2 (,) [minBound .. maxBound] [minBound .. maxBound]
  pure $ Hole (Card r1 s1) (Card r2 s2)


-- | Set of ranks in a ['Card'] (with no duplicates)
--
-- >>> ranks cs
-- fromList [Five,Six,Seven,Ten,Ace]
ranks :: [Card] -> Set.Set Rank
ranks cs = Set.fromList $ rank <$> cs

-- | Set of suits in a hand (with no duplicates)
--
-- >>> suits cs
-- fromList [Clubs,Hearts,Spades]
suits :: [Card] -> Set.Set Suit
suits cs = Set.fromList $ suit <$> cs
