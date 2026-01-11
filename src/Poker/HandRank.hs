{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Evaluation of a standard holdem poker hand. The evaluators work for 5 and 7 card hands.
module Poker.HandRank
  ( -- * Usage
    -- $usage

    -- * storable vector of 'Card's Hand Ranking
    HandRank (..),
    allHandRanks,

    -- * 'Cards' Hand Ranking
    handRank,
    straight,
    flush,
    kind,
    rankCountV,
    rankCount,
    suitRanks,

    -- * Evaluation
    mapLexiHR,
    lexiToHR,
    mapHRLexi,
    hrToLexi,
    handValues,
    hvs7Write,
    hvs7,
    lookupHR,
    lookupHRUnsorted,
    lookupHRs,
    lookupHRsUnsorted,
    sort,
  )
where

import Control.Applicative
import Control.DeepSeq
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Tuple
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Storable (Storable)
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.MMap
import Data.Vector.Storable.Mutable qualified as SM
import Data.Word
import GHC.Exts hiding (toList)
import GHC.Generics
import Optics.Core
import Poker.Card (rank, suit)
import Poker.Card.Storable
import Poker.Lexico
import Prettyprinter
import Prelude

-- $setup
--
-- >>> import Poker.Card
-- >>> import Poker.Card.Storable
-- >>> import Poker.Lexico
-- >>> import Optics.Core
-- >>> import Prettyprinter
-- >>> import Data.Bifunctor
-- >>> import qualified Data.Vector.Storable as S
-- >>> import qualified Data.List as List
-- >>> import Data.Ord (Down)
-- >>> import qualified Data.Map.Strict as Map
-- >>> cs = review cardsI [Card Ace Hearts,Card Seven Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades]
-- >>> css = review cards2I [[Card Ace Hearts,Card Seven Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades], [Card Ten Clubs, Card Five Spades,Card Ten Hearts,Card Five Spades,Card Six Clubs, Card Seven Hearts,Card Six Spades]]
-- >>> s <- hvs7

-- | 5 card standard poker rankings
--
-- >>> pretty $ handRank cs
-- TwoPair Seven Six Ace
data HandRank
  = HighCard RankS RankS RankS RankS RankS
  | OnePair RankS RankS RankS RankS
  | TwoPair RankS RankS RankS
  | ThreeOfAKind RankS RankS RankS
  | Straight RankS
  | Flush RankS RankS RankS RankS RankS
  | FullHouse RankS RankS
  | FourOfAKind RankS RankS
  | StraightFlush RankS
  deriving (Eq, Ord, Show, Generic, NFData)

instance Pretty HandRank where
  pretty (HighCard r0 r1 r2 r3 r4) = hsep ["HighCard", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1, (unsafeViaShow . view rankI) r2, (unsafeViaShow . view rankI) r3, (unsafeViaShow . view rankI) r4]
  pretty (OnePair r0 r1 r2 r3) = hsep ["OnePair", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1, (unsafeViaShow . view rankI) r2, (unsafeViaShow . view rankI) r3]
  pretty (TwoPair r0 r1 r2) = hsep ["TwoPair", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1, (unsafeViaShow . view rankI) r2]
  pretty (ThreeOfAKind r0 r1 r2) = hsep ["ThreeOfAKind", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1, (unsafeViaShow . view rankI) r2]
  pretty (Straight r0) = hsep ["Straight", (unsafeViaShow . view rankI) r0]
  pretty (Flush r0 r1 r2 r3 r4) = hsep ["Flush", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1, (unsafeViaShow . view rankI) r2, (unsafeViaShow . view rankI) r3, (unsafeViaShow . view rankI) r4]
  pretty (FullHouse r0 r1) = hsep ["FullHouse", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1]
  pretty (FourOfAKind r0 r1) = hsep ["FourOfAKind", (unsafeViaShow . view rankI) r0, (unsafeViaShow . view rankI) r1]
  pretty (StraightFlush r0) = hsep ["StraightFlush", (unsafeViaShow . view rankI) r0]

-- | Convert a CardsS to a HandRank
--
-- >>> pretty $ handRank cs
-- TwoPair Seven Six Ace
handRank :: CardsS -> HandRank
handRank cs =
  fromMaybe
    (kind (cardRanksSWithDups cs))
    ( flush cs
        <|> straight (cardRanksS cs)
    )
{-# INLINE handRank #-}

-- | enumeration of all possible HandRanks, in ascending order.
--
-- >>> length allHandRanks
-- 7462
allHandRanks :: [HandRank]
allHandRanks =
  [ HighCard a b c d e
  | a <- ranks,
    b <- ranksLT a,
    c <- ranksLT b,
    d <- ranksLT c,
    e <- ranksLT d,
    not (a == s b && b == s c && c == s d && d == s e),
    not (a == RankS 12 && [b, c, d, e] == [RankS 3, RankS 2, RankS 1, RankS 0])
  ]
    ++ [ OnePair a b c d
       | a <- ranks,
         b <- ranks,
         a /= b,
         c <- ranksLT b,
         a /= c,
         d <- ranksLT c,
         a /= d
       ]
    ++ [ TwoPair a b c
       | a <- ranks,
         b <- ranksLT a,
         c <- ranks,
         a /= c,
         b /= c
       ]
    ++ [ ThreeOfAKind a b c
       | a <- ranks,
         b <- ranks,
         a /= b,
         c <- ranksLT b,
         a /= c
       ]
    ++ [Straight f | f <- ranksGE (RankS 3)]
    ++ [ Flush a b c d e
       | a <- ranks,
         b <- ranksLT a,
         c <- ranksLT b,
         d <- ranksLT c,
         e <- ranksLT d,
         not (a == s b && b == s c && c == s d && d == s e),
         not (a == RankS 12 && [b, c, d, e] == [RankS 3, RankS 2, RankS 1, RankS 0])
       ]
    ++ [FullHouse a b | a <- ranks, b <- ranks, a /= b]
    ++ [FourOfAKind a b | a <- ranks, b <- ranks, a /= b]
    ++ [StraightFlush f | f <- ranksGE (RankS 3)]
  where
    s (RankS 12) = RankS 0
    s (RankS x) = RankS (x + 1)
    ranks = RankS <$> [0 .. 12]
    ranksLT (RankS 0) = []
    ranksLT (RankS x) = RankS <$> [0 .. (x - 1)]
    ranksGE (RankS x) = RankS <$> [x .. 12]

-- | Check for a straight in a rankS with no duplicates.
--
-- >>> pretty <$> (straight $ review ranksI [Ace, King, Queen, Five, Four, Three, Two])
-- Just Straight Five
--
-- >>> straight (cardRanksS cs)
-- Nothing
straight :: RanksS -> Maybe HandRank
straight rs = Straight <$> run rs

run :: RanksS -> Maybe RankS
run r@(RanksS rs) = case S.head rs of
  12 -> run5 r <|> bool Nothing (Just (RankS 3)) (run4 (RanksS $ S.tail rs) == Just (RankS 3))
  _ -> run5 r

runs :: RanksS -> [(RankS, Int)]
runs (RanksS rs) = first RankS <$> done (foldl' step (Nothing, []) (S.toList rs))
  where
    step (Nothing, _) r = (Just (r, r), [])
    step (Just (r1, r0), xs) r =
      bool
        -- if gapped then reset, remember old gap
        (Just (r, r), (r0, fromEnum r0 - fromEnum r1 + 1) : xs)
        -- if one less then do nothing
        (Just (r, r0), xs)
        (fromEnum r + 1 == fromEnum r1)
    done (Nothing, xs) = xs
    done (Just (r1, r0), xs) = (r0, fromEnum r0 - fromEnum r1 + 1) : xs

run5 :: RanksS -> Maybe RankS
run5 rs = listToMaybe $ fst <$> filter ((>= 5) . snd) (runs rs)

run4 :: RanksS -> Maybe RankS
run4 rs = listToMaybe $ fst <$> filter ((>= 4) . snd) (runs rs)

-- | check Flush on storable cards
--
-- >>> pretty <$> (flush $ review cardsI [Card Ace Hearts, Card Seven Clubs, Card Seven Spades, Card Five Hearts, Card Four Hearts, Card Three Hearts, Card Two Hearts])
-- Just StraightFlush Five
--
-- >>> pretty <$> (flush $ review cardsI $ [Card Ace Hearts, Card Seven Clubs, Card Seven Spades, Card Six Hearts, Card Four Hearts, Card Three Hearts, Card Two Hearts])
-- Just Flush Ace Six Four Three Two
--
-- >>> flush cs
-- Nothing
flush :: CardsS -> Maybe HandRank
flush cs =
  case second (sortOn Down) <$> filter ((>= 5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (run (RanksS $ S.fromList $ unwrapRankS <$> rs))
    _ -> Nothing

-- | Group Ranks by Suit
--
-- >>>  bimap (view suitI) (fmap (view rankI)) <$> suitRanks cs
-- [(Clubs,[Six]),(Hearts,[Ace,Ten,Seven]),(Spades,[Seven,Five,Six])]
suitRanks :: CardsS -> [(SuitS, [RankS])]
suitRanks cs =
  fmap (bimap (review suitI) (fmap (review rankI))) $
    Map.toList $
      Map.fromListWith (flip (<>)) $
        fmap (\x -> (suit x, [rank x])) (view cardsI cs)

-- | compute Kinds on storable ranks
--
-- When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> pretty $ kind $ review ranksI [Ace, Ace, Ace, Ace, Two]
-- FourOfAKind Ace Two
--
-- >>> pretty $ kind $ review ranksI [Ace, Ace, Ace, Two, Two]
-- FullHouse Ace Two
--
-- >>> pretty $ kind $ review ranksI [Ace, Ace, Ace, Five, Two]
-- ThreeOfAKind Ace Five Two
--
-- >>> pretty $ kind $ review ranksI [Ace, Ace, Five, Five, Two]
-- TwoPair Ace Five Two
--
-- >>> pretty $ kind $ review ranksI [Ace, Ace, Six, Five, Two]
-- OnePair Ace Six Five Two
--
-- >>> pretty $ kind $ review ranksI [Ace, King, Six, Five, Two]
-- HighCard Ace King Six Five Two
kind :: RanksS -> HandRank
kind rs =
  case rankCount rs of
    ((r0, 4) : (r1, _) : _) -> FourOfAKind r0 r1
    ((r0, 3) : (r1, 3) : _) -> FullHouse r0 r1
    ((r0, 3) : (r1, 2) : _) -> FullHouse r0 r1
    ((r0, 3) : (r1, 1) : (r2, 1) : _) -> ThreeOfAKind r0 r1 r2
    ((r0, 2) : (r1, 2) : (r2, 2) : _) -> TwoPair r0 r1 r2
    ((r0, 2) : (r1, 2) : (r2, 1) : _) -> TwoPair r0 r1 r2
    ((r0, 2) : (r1, 1) : (r2, 1) : (r3, 1) : _) -> OnePair r0 r1 r2 r3
    ((r0, 1) : (r1, 1) : (r2, 1) : (r3, 1) : (r4, 1) : _) -> HighCard r0 r1 r2 r3 r4
    _ -> error ("bad Rank list: " <> show rs)

-- | Count of all ranks, as a 13 element vector
--
-- >>> rankCountV (cardRanksSWithDups cs)
-- [0,0,0,1,2,2,0,0,1,0,0,0,1]
rankCountV :: RanksS -> S.Vector Word8
rankCountV (RanksS rs) = S.create $ do
  v <- SM.replicate 13 (0 :: Word8)
  S.mapM_ (SM.modify v (+ 1)) (S.map fromEnum rs)
  pure v

-- | Count of all ranks, as a list.
--
-- >>> first (view rankI) <$> (rankCount $ (cardRanksSWithDups cs))
-- [(Seven,2),(Six,2),(Ace,1),(Ten,1),(Five,1)]
rankCount :: RanksS -> [(RankS, Word8)]
rankCount rs =
  fmap (first (RankS . fromIntegral)) $ sortOn (Down . swap) $ toList $ V.imapMaybe (\i a -> bool Nothing (Just (i, a)) (a /= 0)) (S.convert $ rankCountV rs)

-- | vector of hand values indexed by lexigraphic order for n-card combinations.
--
-- >>> (fmap (hrToLexi . handRank . CardsS . S.fromList) (combinationsR 5 $ S.toList $ unwrapCardsS allCardsS)) List.!! 1000000
-- 645
--
-- >>> fromLexiPosR 52 5 1000000 & fmap fromIntegral & S.fromList & CardsS & handRank & hrToLexi
-- 645
handValues :: Int -> S.Vector Word16
handValues n = S.fromList $ fmap (hrToLexi . handRank . CardsS . S.fromList) (combinationsR n [0 .. 51])

-- | write handRank vector to an mmap'ped file
hvsWrite :: Int -> FilePath -> IO ()
hvsWrite n f = writeMMapVector f (handValues n)

-- | write the hvs7 vector to a file
--
-- Takes 5 minutes.
hvs7Write :: IO ()
hvs7Write = hvsWrite 7 "other/hvs7.vec"

-- | Vector of hand values for 7 card combinations in lexicographic order
--
-- >>> s <- hvs7
-- >>> S.length s
-- 133784560
-- >>> pretty $ lexiToHR (s S.! 133784559)
-- FourOfAKind Two Three
-- >>> pretty $ lexiToHR (s S.! 0)
-- FourOfAKind Ace King
hvs7 :: IO (S.Vector Word16)
hvs7 = unsafeMMapVector "other/hvs7.vec" Nothing

-- | lexicographic index to HandRank map
mapLexiHR :: Map.Map Word16 HandRank
mapLexiHR = Map.fromList (zip [(0 :: Word16) ..] allHandRanks)

-- | lexicographic index to HandRank
--
-- > lexiToHR . hrToLexi == id
--
-- >>> pretty $ lexiToHR 4301
-- TwoPair Seven Six Ace
lexiToHR :: Word16 -> HandRank
lexiToHR = (Map.!) mapLexiHR

-- | HandRank to reverse lexicographic Word16 index map
mapHRLexi :: Map.Map HandRank Word16
mapHRLexi = Map.fromList (zip allHandRanks [(0 :: Word16) ..])

-- | HandRank to lexicographic index
--
-- > hrToLexi . lexiToHR == id
--
-- >>> hrToLexi $ handRank cs
-- 4301
hrToLexi :: HandRank -> Word16
hrToLexi = (Map.!) mapHRLexi

-- | look up the HandRank of some cards.
--
-- >>> xs = CardsS $ S.fromList $ [15,17,19,20,23,32,48]
-- >>> pretty xs
-- 5s6d6s7c7sTcAc
--
-- >>> pretty $ handRank xs
-- TwoPair Seven Six Ace
--
-- >>> s <- hvs7
-- >>> pretty $ lexiToHR (lookupHR s xs)
-- TwoPair Seven Six Ace
lookupHR :: S.Vector Word16 -> CardsS -> Word16
lookupHR s (CardsS v) = s S.! toLexiPosR 52 7 v

-- | sort a 'Storable' 'Vector.Storable.Vector'
sort :: (Ord a, Storable a) => S.Vector a -> S.Vector a
sort xs = S.create $ do
  xs' <- S.thaw xs
  Intro.sort xs'
  pure xs'

-- | Version for unsorted cards
--
-- >>> cs & unwrapCardsS & S.reverse & CardsS & lookupHRUnsorted s & lexiToHR & pretty
-- TwoPair Seven Six Ace
lookupHRUnsorted :: S.Vector Word16 -> CardsS -> Word16
lookupHRUnsorted s (CardsS v) = s S.! toLexiPosR 52 7 (sort v)

-- | look up the HandRank of a bunch of cards. CardsS must be sorted in ascending order.
--
-- >>> cssSorted = css & applyV (CardsS . sort . unwrapCardsS) & review cardsS7V
-- >>> pretty <$>  fmap lexiToHR $ S.toList $ lookupHRs s cssSorted
-- [TwoPair Seven Six Ace, TwoPair Ten Six Two]
lookupHRs :: S.Vector Word16 -> Cards2S -> S.Vector Word16
lookupHRs s = apply (lookupHR s)

-- | look up the HandRank of a bunch of cards. CardsS can be unsorted.
--
-- >>> pretty <$>  fmap lexiToHR $ S.toList $ lookupHRsUnsorted s css
-- [TwoPair Seven Six Ace, TwoPair Ten Six Two]
lookupHRsUnsorted :: S.Vector Word16 -> Cards2S -> S.Vector Word16
lookupHRsUnsorted s = apply (lookupHRUnsorted s)
