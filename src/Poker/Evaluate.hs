{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

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
module Poker.Evaluate
  ( -- * Usage
    -- $usage

    -- * ['Card'] Hand Ranking
    HandRank (..),
    handRank,
    straight,
    flush,
    kind,
    rankCount,
    suitRanks,

    -- * 'CardsS' Hand Ranking
    handRankS,
    straightS,
    flushS,
    kindS,
    rankCountS',
    rankCountS,
    suitRanksS,

    -- * Combinatorial Math
    combinations,
    combinationsR,
    binom,
    binomR,
    toLexiPosR,
    toLexiPosRS,
    fromLexiPosR,

    -- * Pre-Evaluation
    handRankL,
    sortS,
    mapHRValue,
    mapValueHR,
    handValues,
    hvs7Write,
    hvs7,
    allHandRanks,
    lookupHR,
    lookupHRUnsorted,
    lookupHRUnsafe,
    lookupHRs,
    lookupHRsUnsafe,
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Data.Tuple
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import Data.Vector.Storable.MMap
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import GHC.Exts hiding (toList)
import GHC.Generics hiding (from, to)
import Poker hiding (fromList)
import Poker.Card.Storable
import System.IO.Unsafe (unsafePerformIO)
import Prelude

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
      not (a == succ b && b == succ c && c == succ d && d == s e),
      not (a == Ace && [b, c, d, e] == [Five, Four, Three, Two])
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
    ++ [Straight f | f <- ranksGE Five]
    ++ [ Flush a b c d e
         | a <- ranks,
           b <- ranksLT a,
           c <- ranksLT b,
           d <- ranksLT c,
           e <- ranksLT d,
           not (a == succ b && b == succ c && c == succ d && d == s e),
           not (a == Ace && [b, c, d, e] == [Five, Four, Three, Two])
       ]
    ++ [FullHouse a b | a <- ranks, b <- ranks, a /= b]
    ++ [FourOfAKind a b | a <- ranks, b <- ranks, a /= b]
    ++ [StraightFlush f | f <- ranksGE Five]
  where
    s Ace = Two
    s other = succ other
    ranks = [Two .. Ace]
    ranksLT Two = []
    ranksLT rank = [Two .. pred rank]
    ranksGE rank = reverse [Ace, King .. rank]

-- | compute a HandRank from a list of Cards.
--
-- Should work for 5 and 7 hand variants.
--
-- >>> handRank cs
-- TwoPair Seven Six Ace
handRank :: [Card] -> HandRank
handRank cs =
  fromMaybe
    (kind (rank <$> cs'))
    ( flush cs'
        <|> straight cs'
    )
  where
    cs' = sortOn Down cs

-- | 5 consecutive card check
--
-- Special rules for an Ace, which can be counted as high or low.
--
-- > run [Ace, King, Queen, Jack, Ten, Nine, Eight]
-- Just Ace
--
-- > run [Ace, King, Queen, Jack, Ten, Eight, Seven]
-- Just Ace
--
-- > run [Ace, King, Queen, Five, Four, Three, Two]
-- Just Five
--
-- > run [Ace, King, Queen, Six, Four, Three, Two]
-- Nothing
run :: [Rank] -> Maybe Rank
run [] = Nothing
run rs@(Ace : rs') = run5 rs <|> bool Nothing (Just Five) (run4 rs' == Just Five)
run rs = run5 rs

-- | Check for a straight. Note holdem rules for an Ace, which can be counted as high or low.
--
-- >>> straight $ (\r -> Card r Heart) <$> [Ace, King, Queen, Five, Four, Three, Two]
-- Just (Straight Five)
--
-- >>> straight cs
-- Nothing
straight :: [Card] -> Maybe HandRank
straight cs = Straight <$> run (Set.toDescList $ ranks cs)

run5 :: [Rank] -> Maybe Rank
run5 rs = listToMaybe $ fst <$> filter ((>= 5) . snd) (runs rs)

run4 :: [Rank] -> Maybe Rank
run4 rs = listToMaybe $ fst <$> filter ((>= 4) . snd) (runs rs)

runs :: [Rank] -> [(Rank, Int)]
runs rs = done (foldl' step (Nothing, []) rs)
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

-- | Check if a hand is a Flush or StraightFlush
--
-- >>> flush [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (StraightFlush Five)
--
-- >>> flush [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Six Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (Flush Ace Six Four Three Two)
--
-- >>> flush cs
-- Nothing
flush :: [Card] -> Maybe HandRank
flush cs =
  case filter ((>= 5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (run rs)
    _ -> Nothing

-- | Group Ranks by Suit
--
-- >>> suitRanks cs
-- [(Club,[Six]),(Heart,[Ace,Ten,Seven]),(Spade,[Seven,Five,Six])]
suitRanks :: [Card] -> [(Suit, [Rank])]
suitRanks cs =
  Map.toList $
    Map.fromListWith (flip (<>)) $
      fmap (\(Card r s) -> (s, [r])) cs

-- | count of Ranks from a sorted list
--
-- >>> import Data.Ord
-- >>> import Data.List (sortOn)
-- >>> rankCount (rank <$> (sortOn Down cs))
-- [(Seven,2),(Six,2),(Ace,1),(Ten,1),(Five,1)]
rankCount :: [Rank] -> [(Rank, Int)]
rankCount rs =
  sortOn (Down . swap) $
    Map.toList $
      Map.fromDescListWith (+) $
        fmap (,1) rs

-- | When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> kind [Ace, Ace, Ace, Ace, Two]
-- FourOfAKind Ace Two
--
-- >>> kind [Ace, Ace, Ace, Two, Two]
-- FullHouse Ace Two
--
-- >>> kind [Ace, Ace, Ace, Five, Two]
-- ThreeOfAKind Ace Five Two
--
-- >>> kind [Ace, Ace, Five, Five, Two]
-- TwoPair Ace Five Two
--
-- >>> kind [Ace, Ace, Six, Five, Two]
-- OnePair Ace Six Five Two
--
-- >>> kind [Ace, King, Six, Five, Two]
-- HighCard Ace King Six Five Two
kind :: [Rank] -> HandRank
kind cs = case rankCount cs of
  ((r0, 4) : (r1, _) : _) -> FourOfAKind r0 r1
  ((r0, 3) : (r1, 3) : _) -> FullHouse r0 r1
  ((r0, 3) : (r1, 2) : _) -> FullHouse r0 r1
  ((r0, 3) : (r1, 1) : (r2, 1) : _) -> ThreeOfAKind r0 r1 r2
  ((r0, 2) : (r1, 2) : (r2, 2) : _) -> TwoPair r0 r1 r2
  ((r0, 2) : (r1, 2) : (r2, 1) : _) -> TwoPair r0 r1 r2
  ((r0, 2) : (r1, 1) : (r2, 1) : (r3, 1) : _) -> OnePair r0 r1 r2 r3
  ((r0, 1) : (r1, 1) : (r2, 1) : (r3, 1) : (r4, 1) : _) -> HighCard r0 r1 r2 r3 r4
  _ -> error ("bad Rank list: " <> show cs)

-- | CardsS version of handRank
--
-- >>> handRankS (from cardsS cs)
-- TwoPair Seven Six Ace
handRankS :: CardsS -> HandRank
handRankS cs =
  fromMaybe
    (kindS (toRanksS cs))
    ( flushS cs
        <|> straightS (ranksSet cs)
    )

-- | Check straight on storable ranks
--
-- >>> straightS $ from ranksS [Ace, King, Queen, Five, Four, Three, Two]
-- Just (Straight Five)
--
-- >>> straightS (ranksSet $ from cardsS cs)
-- Nothing
straightS :: RanksS -> Maybe HandRank
straightS rs = Straight <$> runS rs

runS :: RanksS -> Maybe Rank
runS r@(RanksS rs) = case S.head rs of
  12 -> run5S r <|> bool Nothing (Just Five) (run4S (RanksS $ S.tail rs) == Just Five)
  _ -> run5S r

runsS :: RanksS -> [(Rank, Int)]
runsS (RanksS rs) = done (foldl' step (Nothing, []) (toEnum . fromEnum <$> S.toList rs))
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

run5S :: RanksS -> Maybe Rank
run5S rs = listToMaybe $ fst <$> filter ((>= 5) . snd) (runsS rs)

run4S :: RanksS -> Maybe Rank
run4S rs = listToMaybe $ fst <$> filter ((>= 4) . snd) (runsS rs)

-- | check Flush on storable cards
--
-- >>> flushS $ from cardsS [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (StraightFlush Five)
--
-- >>> flushS $ from cardsS $ [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Six Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (Flush Ace Six Four Three Two)
--
-- >>> flushS (from cardsS cs)
-- Nothing
flushS :: CardsS -> Maybe HandRank
flushS cs =
  case second (sortOn Down) <$> filter ((>= 5) . length . snd) (suitRanksS cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (run rs)
    _ -> Nothing

-- | Group Ranks by Suit
--
-- >>> suitRanksS (from cardsS cs)
-- [(Club,[Six]),(Heart,[Ace,Ten,Seven]),(Spade,[Seven,Five,Six])]
suitRanksS :: CardsS -> [(Suit, [Rank])]
suitRanksS cs =
  Map.toList $
    Map.fromListWith (flip (<>)) $
      fmap (\(Card r s) -> (s, [r])) (to cardsS cs)

-- | compute Kinds on storable ranks
--
-- When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> kindS $ from ranksS [Ace, Ace, Ace, Ace, Two]
-- FourOfAKind Ace Two
--
-- >>> kindS $ from ranksS [Ace, Ace, Ace, Two, Two]
-- FullHouse Ace Two
--
-- >>> kindS $ from ranksS [Ace, Ace, Ace, Five, Two]
-- ThreeOfAKind Ace Five Two
--
-- >>> kindS $ from ranksS [Ace, Ace, Five, Five, Two]
-- TwoPair Ace Five Two
--
-- >>> kindS $ from ranksS [Ace, Ace, Six, Five, Two]
-- OnePair Ace Six Five Two
--
-- >>> kindS $ from ranksS [Ace, King, Six, Five, Two]
-- HighCard Ace King Six Five Two
kindS :: RanksS -> HandRank
kindS rs =
  case rankCountS rs of
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
-- >>> rankCountS' (from ranksS $ rank <$> cs)
-- [0,0,0,1,2,2,0,0,1,0,0,0,1]
rankCountS' :: RanksS -> S.Vector Word8
rankCountS' (RanksS rs) = S.create $ do
  v <- SM.replicate 13 (0 :: Word8)
  S.mapM_ (SM.modify v (+ 1)) (S.map fromEnum rs)
  pure v

-- | Count of all ranks, as a list.
--
-- >>> rankCountS $ toRanksS (from cardsS cs)
-- [(Seven,2),(Six,2),(Ace,1),(Ten,1),(Five,1)]
rankCountS :: RanksS -> [(Rank, Word8)]
rankCountS rs =
  fmap (first toEnum) $ sortOn (Down . swap) $ toList $ V.imapMaybe (\i a -> bool Nothing (Just (i, a)) (a /= 0)) (S.convert $ rankCountS' rs)


-- | @combinations k xs@ generates a list of k-combinations from xs
--
-- >>> combinations 2 [0..4]
-- [[0,1],[0,2],[0,3],[0,4],[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations m l = [x : ys | x : xs <- List.tails l, ys <- combinations (m - 1) xs]

-- | List of k-element combinations in reverse lexicographic order.
--
-- >>> combinationsR 2 [0..4]
-- [[3,4],[2,4],[1,4],[0,4],[2,3],[1,3],[0,3],[1,2],[0,2],[0,1]]
--
-- > List.length (combinationsR 5 [0..51]) == binom 52 5
-- 2598960
combinationsR :: Int -> [a] -> [[a]]
combinationsR 0 _ = [[]]
combinationsR m l = reverse <$> combinations m (reverse l)

-- | Given a combination, what is its position in reverse lexicographic ordering of all combinations.
--
--  <https://math.stackexchange.com/questions/1368526/fast-way-to-get-a-combination-given-its-position-in-reverse-lexicographic-or/1368570#1368570 stackexchange question>
--
-- >>> toLexiPosR 52 2 [50,51]
-- 0
--
-- >>>  toLexiPosR 52 2 [0,1]
-- 1325
--
-- >>> toLexiPosR 5 2 <$> combinationsR 2 [0..4]
-- [0,1,2,3,4,5,6,7,8,9]
toLexiPosR :: Int -> Int -> [Int] -> Int
toLexiPosR n k xs = binom n k - 1 - sum (zipWith binom xs [1 ..])

-- | reverse lexicographic position of a storable vector with enumerated binom function
--
-- > toLexiPosRS n k s = binom n k - 1 - S.sum (S.imap (\i a -> binom a (1+i)) s)
-- > toLexiPosR == toLexiPosRS . S.fromList
--
-- >>> toLexiPosRS 5 2 <$> S.fromList <$> combinationsR 2 [0..4]
-- [0,1,2,3,4,5,6,7,8,9]
toLexiPosRS :: Int -> Int -> S.Vector Int -> Int
toLexiPosRS n k s = binom n k - 1 - S.sum (S.imap (\i a -> binom a (1 + i)) s)

-- | Given a reverse lexicographic position, what was the combination?
--
-- >>> (\xs -> xs == fmap (fromLexiPosR 5 2 . toLexiPosR 5 2) xs) (combinations 2 [0..4])
-- True
--
-- >>> ((combinationsR 5 allCards) List.!! 1000000) == (fmap toEnum (fromLexiPosR 52 5 1000000) :: [Card])
-- True
fromLexiPosR :: Int -> Int -> Int -> [Int]
fromLexiPosR n k p = go (n - 1) k (binom n k - 1 - p) []
  where
    go n' k' p' xs =
      bool
        ( bool
            (go (n' - 1) k' p' xs)
            (go (n' - 1) (k' - 1) (p' - binom n' k') (n' : xs))
            (p' >= binom n' k')
        )
        xs
        (length xs == k)

-- | binomial equation
--
-- The number of 7-card combinations for a 52 card deck is:
--
-- >>> binom 52 7
-- 133784560
binom :: Int -> Int -> Int
binom _ 0 = 1
binom 0 _ = 0
binom n k = product [(n - k + 1) .. n] `div` product [1 .. k]

-- | recursive version of binomial equation
--
-- >>> binomR 52 7
-- 133784560
binomR :: Int -> Int -> Int
binomR _ 0 = 1
binomR 0 _ = 0
binomR n k = binomR (n - 1) (k - 1) * n `div` k

-- | vector of hand values indexed by lexigraphic order for n-card combinations.
--
-- >>> fmap ((Map.!) mapHRValue . handRank) (combinationsR 5 allCards) List.!! 1000000
-- 645
--
-- >>> ((Map.!) mapHRValue) (handRank (toEnum <$> (fromLexiPosR 52 5 1000000) :: [Card]))
-- 645
handValues :: Int -> S.Vector Word16
handValues n = S.fromList $ fmap ((mapHRValue Map.!) . handRankS . CardsS . S.fromList) (combinationsR n [0 .. 51])

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
-- >>> handRankL (from cardsS $ List.sort cs)
-- 4301
handRankL :: CardsS -> Word16
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
-- >>> ((Map.!) mapValueHR) $ lookupHR s (CardsS $ S.fromList xs)
-- TwoPair Seven Six Ace
--
lookupHR :: S.Vector Word16 -> CardsS -> Word16
lookupHR s (CardsS v) = s S.! toLexiPosRS 52 7 (S.map fromEnum v)

-- | sort a 'Storable' 'Vector.Storable.Vector'
sortS :: (Ord a, Storable a) => S.Vector a -> S.Vector a
sortS xs = S.create $ do
  xs' <- S.thaw xs
  Intro.sort xs'
  pure xs'

-- | Version for unsorted cards
--
-- >>> (Map.!) mapValueHR . lookupHRUnsorted s . from cardsS <$> css
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRUnsorted :: S.Vector Word16 -> CardsS -> Word16
lookupHRUnsorted s (CardsS v) = s S.! toLexiPosRS 52 7 (sortS $ S.map fromEnum v)

-- | version hiding the IO call for hvs7
--
-- >>> (Map.!) mapValueHR . lookupHRUnsafe . from cardsS <$> (List.sort <$> css)
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRUnsafe :: CardsS -> Word16
lookupHRUnsafe = lookupHR (unsafePerformIO hvs7)

-- | look up the HandRank of a bunch of cards. Cards must be sorted in ascending order.
--
-- >>> fmap ((Map.!) mapValueHR) $ S.toList $ lookupHRs s (from cardsS7L (List.sort <$> css))
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRs :: S.Vector Word16 -> Cards2S -> S.Vector Word16
lookupHRs s = applyS (lookupHR s)

-- | version hiding IO
--
-- >>> fmap ((Map.!) mapValueHR) $ S.toList $ lookupHRsUnsafe (from cardsS7L (List.sort <$> css))
-- [TwoPair Seven Six Ace,TwoPair Ten Six Two]
lookupHRsUnsafe :: Cards2S -> S.Vector Word16
lookupHRsUnsafe = lookupHRs (unsafePerformIO hvs7)
