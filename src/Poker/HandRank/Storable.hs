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
{-# LANGUAGE DeriveGeneric #-}

-- | Evaluation of a standard holdem poker hand. The evaluators work for 5 and 7 card hands.
--
-- The module supplies:
--
-- - 'handRank' and helpers for evaluation of a ['Card']
--
-- - 'handRankS' and helpers for evaluation of a 'CardsS'
--
-- - 'handRankL' and helpers for lookup of a pre-evaluated vector of the ranking of all possible 7 card hands.
--
module Poker.HandRank.Storable
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
    rankCount,
    suitRanks,

    -- * Evaluation
    handRankL,
    mapHRValue,
    mapValueHR,
    handValues,
    hvs7Write,
    hvs7,

    lookupHR,
    lookupHRUnsorted,
    lookupHRUnsafe,
    lookupHRs,
    lookupHRsUnsafe,

    sort,
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Data.Tuple
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import Data.Vector.Storable.MMap
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import GHC.Exts hiding (toList)
import Poker.Card.Storable
import System.IO.Unsafe (unsafePerformIO)
import Prelude
import Poker.Lexico
import GHC.Generics
import qualified Data.Vector as V
import Optics.Core
import Poker.HandRank.List (rankI, cardsI, suitI)


-- $usage
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> import qualified Data.List as List
-- >>> import Data.Ord (Down)
-- >>> import qualified Data.Map.Strict as Map
-- >>> let cs = [Card Ace Heart,Card Seven Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]
--
-- The pre-evaluated storable vector
--
-- >>> s <- hvs7
-- >>> :t s
-- s :: S.Vector GHC.Word.Word16

-- $setup
--
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Prettyprinter
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Storable as S
-- >>> import qualified Data.List as List
-- >>> import Data.Ord (Down)
-- >>> import qualified Data.Map.Strict as Map
-- >>> let cs = [Card Ace Heart,Card Seven Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty $ from cardsS7L css
-- [Ah7sTh5s6c7h6s, Tc5sTh5s6c7h6s]
--
-- The pre-evaluated storable vector
--
-- >>> s <- hvs7
-- >>> :t s
-- s :: S.Vector GHC.Word.Word16

-- | 5 card standard poker rankings
--
-- >>> handRank <$> css
-- [TwoPair Seven Six Ace,TwoPair Ten Six Five]
data HandRank
  = HighCard Rank Rank Rank Rank Rank
  | OnePair Rank Rank Rank Rank
  | TwoPair Rank Rank Rank
  | ThreeOfAKind Rank Rank Rank
  | Straight Rank
  | Flush Rank Rank Rank Rank Rank
  | FullHouse Rank Rank
  | FourOfAKind Rank Rank
  | StraightFlush Rank
  deriving (Eq, Ord, Show, Generic)

-- | CardsS version of handRank
--
-- >>> handRank (from cardsS cs)
-- TwoPair Seven Six Ace
handRank :: Cards -> HandRank
handRank cs =
  fromMaybe
    (kind (toRanks cs))
    ( flush cs
        <|> straight (ranksSet cs)
    )

-- | enumeration of all possible HandRanks, in ascending order.
--
-- >>> length allHandRanks
-- 7462
--
allHandRanks :: [HandRank]
allHandRanks =
  [ HighCard a b c d e
    | a <- ranks,
      b <- ranksLT a,
      c <- ranksLT b,
      d <- ranksLT c,
      e <- ranksLT d,
      not (a == succ' b && b == succ' c && c == succ' d && d == s e),
      not (a == Rank 12 && [b, c, d, e] == [Rank 3, Rank 2, Rank 1, Rank 0])
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
    ++ [Straight f | f <- ranksGE (Rank 3)]
    ++ [ Flush a b c d e
         | a <- ranks,
           b <- ranksLT a,
           c <- ranksLT b,
           d <- ranksLT c,
           e <- ranksLT d,
           not (a == succ' b && b == succ' c && c == succ' d && d == s e),
           not (a == Rank 12 && [b, c, d, e] == [Rank 3, Rank 2, Rank 1, Rank 0])
       ]
    ++ [FullHouse a b | a <- ranks, b <- ranks, a /= b]
    ++ [FourOfAKind a b | a <- ranks, b <- ranks, a /= b]
    ++ [StraightFlush f | f <- ranksGE (Rank 3)]
  where
    s (Rank 12) = Rank 0
    s (Rank x) = Rank (x+1)
    ranks = Rank <$> [0 .. 12]
    ranksLT (Rank 0) = []
    ranksLT (Rank x) = Rank <$> [0 .. (x - 1)]
    ranksGE (Rank x) = Rank <$> reverse (12:[x .. 11])
    succ' (Rank 0) = Rank 0
    succ' (Rank x) = Rank (x - 1)

-- | Check straight on storable ranks
--
-- >>> straight $ from ranks [Ace, King, Queen, Five, Four, Three, Two]
-- Just (Straight Five)
--
-- >>> straight (ranksSet $ from cards cs)
-- Nothing
straight :: Ranks -> Maybe HandRank
straight rs = Straight <$> run rs

run :: Ranks -> Maybe Rank
run r@(Ranks rs) = case S.head rs of
  12 -> run5 r <|> bool Nothing (Just (Rank 3)) (run4 (Ranks $ S.tail rs) == Just (Rank 3))
  _ -> run5 r

runs :: Ranks -> [(Rank, Int)]
runs (Ranks rs) = first Rank <$> done (foldl' step (Nothing, []) (S.toList rs))
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

run5 :: Ranks -> Maybe Rank
run5 rs = listToMaybe $ fst <$> filter ((>= 5) . snd) (runs rs)

run4 :: Ranks -> Maybe Rank
run4 rs = listToMaybe $ fst <$> filter ((>= 4) . snd) (runs rs)

-- | check Flush on storable cards
--
-- >>> flush $ from cards [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (StraightFlush Five)
--
-- >>> flush $ from cards $ [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Six Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (Flush Ace Six Four Three Two)
--
-- >>> flush (from cards cs)
-- Nothing
flush :: Cards -> Maybe HandRank
flush cs =
  case second (sortOn Down) <$> filter ((>= 5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (run (Ranks $ S.fromList $ unwrapRank <$> rs))
    _ -> Nothing

-- | Group Ranks by Suit
--
-- >>> suitRanks (from cards cs)
-- [(Club,[Six]),(Heart,[Ace,Ten,Seven]),(Spade,[Seven,Five,Six])]
suitRanks :: Cards -> [(Suit, [Rank])]
suitRanks cs =
  fmap (bimap (review suitI) (fmap (review rankI))) $
  Map.toList $
   Map.fromListWith (flip (<>)) $
      fmap (\x -> (suit x, [rank x])) (review cardsI cs)

-- | compute Kinds on storable ranks
--
-- When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> kind $ from ranks [Ace, Ace, Ace, Ace, Two]
-- FourOfAKind Ace Two
--
-- >>> kind $ from ranks [Ace, Ace, Ace, Two, Two]
-- FullHouse Ace Two
--
-- >>> kind $ from ranks [Ace, Ace, Ace, Five, Two]
-- ThreeOfAKind Ace Five Two
--
-- >>> kind $ from ranks [Ace, Ace, Five, Five, Two]
-- TwoPair Ace Five Two
--
-- >>> kind $ from ranks [Ace, Ace, Six, Five, Two]
-- OnePair Ace Six Five Two
--
-- >>> kind $ from ranks [Ace, King, Six, Five, Two]
-- HighCard Ace King Six Five Two
kind :: Ranks -> HandRank
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
-- >>> rankCountS' (from ranks $ rank <$> cs)
-- [0,0,0,1,2,2,0,0,1,0,0,0,1]
rankCountS' :: Ranks -> S.Vector Word8
rankCountS' (Ranks rs) = S.create $ do
  v <- SM.replicate 13 (0 :: Word8)
  S.mapM_ (SM.modify v (+ 1)) (S.map fromEnum rs)
  pure v

-- | Count of all ranks, as a list.
--
-- >>> rankCount $ toRanks (from cards cs)
-- [(Seven,2),(Six,2),(Ace,1),(Ten,1),(Five,1)]
rankCount :: Ranks -> [(Rank, Word8)]
rankCount rs =
  fmap (first (Rank . fromIntegral)) $ sortOn (Down . swap) $ toList $ V.imapMaybe (\i a -> bool Nothing (Just (i, a)) (a /= 0)) (S.convert $ rankCountS' rs)

-- | vector of hand values indexed by lexigraphic order for n-card combinations.
--
-- >>> fmap ((Map.!) mapHRValue . handRank) (combinationsR 5 allCards) List.!! 1000000
-- 645
--
-- >>> ((Map.!) mapHRValue) (handRank (toEnum <$> (fromLexiPosR 52 5 1000000) :: [Card]))
-- 645
handValues :: Int -> S.Vector Word16
handValues n = S.fromList $ fmap ((mapHRValue Map.!) . handRank . Cards . S.fromList) (combinationsR n [0 .. 51])

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
-- >>> S.length <$> hvs7
-- 133784560
hvs7 :: IO (S.Vector Word16)
hvs7 = unsafeMMapVector "other/hvs7.vec" Nothing

-- | Lookup the value of a hand in the pre-evaluated vector, unsafely.
--
-- >>> handRankL (from cards $ List.sort cs)
-- 4301
handRankL :: Cards -> Word16
handRankL = lookupHRUnsafe

-- | HandRank to reverse lexicographic Word16 index map
--
-- > ((Map.!) mapHRValue) . ((Map.!) mapValueHR) == id
--
-- >>> (Map.!) mapHRValue $ (Map.!) mapValueHR 1000
-- 1000
mapHRValue :: Map.Map HandRank Word16
mapHRValue = Map.fromList (zip allHandRanks [(0 :: Word16) ..])

-- | lexicographic index to HandRank
--
-- >>> s <- hvs7
-- >>> ((Map.!) mapValueHR) (s S.! 133784559)
-- FourOfAKind Two Three
--
-- >>> ((Map.!) mapValueHR) (s S.! 0)
-- FourOfAKind Ace King
mapValueHR :: Map.Map Word16 HandRank
mapValueHR = Map.fromList (zip [(0 :: Word16) ..] allHandRanks)

-- | look up the HandRank of some cards.
--
-- >>> let xs = [15,17,19,20,23,32,48]
-- >>> pretty $ (toEnum <$> xs :: [Card])
-- [5s, 6d, 6s, 7c, 7s, Tc, Ac]
--
-- >>> handRank (toEnum <$> xs :: [Card])
-- TwoPair Seven Six Ace
--
-- >>> s <- hvs7
-- >>> mapValueHR Map.! (s S.! 0)
-- FourOfAKind Ace King
--
-- >>> mapValueHR Map.! (s S.! 133784559)
-- FourOfAKind Two Three
--
-- >>> s <- hvs7
-- >>> ((Map.!) mapValueHR) $ lookupHR s (Cards $ S.fromList xs)
-- TwoPair Seven Six Ace
--
lookupHR :: S.Vector Word16 -> Cards -> Word16
lookupHR s (Cards v) = s S.! fromIntegral (toLexiPosR 52 7 v)

-- | sort a 'Storable' 'Vector.Storable.Vector'
sort :: (Ord a, Storable a) => S.Vector a -> S.Vector a
sort xs = S.create $ do
  xs' <- S.thaw xs
  Intro.sort xs'
  pure xs'

-- | Version for unsorted cards
--
-- >>> (Map.!) mapValueHR . lookupHRUnsorted s . from cards <$> css
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRUnsorted :: S.Vector Word16 -> Cards -> Word16
lookupHRUnsorted s (Cards v) = s S.! fromIntegral (toLexiPosR 52 7 (sort v))

-- | version hiding the IO call for hvs7
--
-- >>> (Map.!) mapValueHR . lookupHRUnsafe . from cards <$> (List.sort <$> css)
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRUnsafe :: Cards -> Word16
lookupHRUnsafe = lookupHR (unsafePerformIO hvs7)

-- | look up the HandRank of a bunch of cards. Cards must be sorted in ascending order.
--
-- >>> fmap ((Map.!) mapValueHR) $ S.toList $ lookupHRs s (from cardsS7L (List.sort <$> css))
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRs :: S.Vector Word16 -> Cards2 -> S.Vector Word16
lookupHRs s = apply (lookupHR s)

-- | version hiding IO
--
-- >>> fmap ((Map.!) mapValueHR) $ S.toList $ lookupHRsUnsafe (from cardsS7L (List.sort <$> css))
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRsUnsafe :: Cards2 -> S.Vector Word16
lookupHRsUnsafe = lookupHRs (unsafePerformIO hvs7)

