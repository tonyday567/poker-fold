{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | __A Shaped Hole__
--
-- A 'Suit' in holdem is unordered; no Suit is better or worse than another when assessing hand ranks. A consequence of the unordered nature of suits is that the set of all 'Hole's, the pair of cards dealt to a player, can be divided into iso-equivalence classes where equivalance is seen from the point of view of hand strength or value in the presence of non-determinism.
--
-- For example, there is no difference pre-flop between being dealt a @AhKc@ and a @AsKd@. Specifically, the probability of forming various hands by the river is exactly the same for both hands. Another example is @AhKc@ and @KcAh@ as the order of the cards doesn't effect the potential hand value.
--
-- It follows then that the Strategy (how should one progress or act in a holdem game) for both of these example 'Hole's is also identical. This is the idea behind a 'StartingHand'.
--
-- A 'StartingHand' can be:
--
-- - a 'Pair' (two 'Card's of the same 'Rank'): which occurs 12 times in a full enumerations of a fresh deck. (4 suits x 3 suits)
--
-- - an 'Offsuited' (two 'Card's of different 'Rank', with the first rank of a 'StartingHand' representing the highest rank): which occurs 24 time ((low ranked dealt first && high rank dealt first) x 4 suits x 3 suits)
--
-- - 'Suited' (two 'Card's of different 'Rank' and the same 'Suit'): which occurs 8 times ((low && high ranked) x 4 suits)
--
-- ![count example](other/count.svg)
--
-- With respect to poker strategy (how should I act), a 'StartingHand' can be considered to be a lossless compression algorithm. Transformations from 'Hole' to 'StartingHand' reduce the necessary enumeration of Strategy from 2652 different possibilities to 169, with no loss of fidelity.
--
--
-- __A Ranged Hole__
--
-- A 'RangedHole' a is a vector of a's representing a value for each possible 'StartingHand' (there are 169 of them). Poker strategy (what should I do) is often expressed in terms of 'StartingHand's. Some examples:
--
-- - "In the big blind, facing an AllIn raise, you should call with your best 20% of hands."
--
-- This can be represented as a RangedHole Bool. "Pair Ace" is likely to be True and "Offsuited Three Two" False.
--
-- - "I think that bet is a complete bluff"
-- A RangedHole Double representing a probability distribution eg 0 for good StartingHands (not bluffs) and a small value for all other shaped hands (bluffs), adding to one.
--
-- - "I always lose with Jacks."
--
-- A statement that can be verified by examining a hand history of the player and dividing up profit and loss into starting hand HoleRanges.
module Poker.Range
  ( -- * Storable StartingHand
    StartingHand (..),
    StartingHandS (..),
    startingHandI,
    toRepHole,
    fromOPS,

    -- * Range
    Range (..),
    rhText,
    handTypeCount,
    any2,
    always,
    allin,
    ordered,
    ops,

    -- * Simulation
    simStartingHandWin,
    winOdds,
    topBs,
    ev,
    evs,
    ev2Ranges,
    rcf,

    -- * Table Interaction
    dealStartingHand,
    dealTableHole,
    tablesB,
    apply,
    applies,

    -- * Saved RangeHoles
    someRanges,
    writeSomeRanges,
    readSomeRanges,

    -- * lpad
    lpad,
    fromHole',
    fromHole'',
    fromHole,
    toHoles,
  )
where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Vector.Storable qualified as S
import GHC.Exts hiding (toList)
import GHC.Read
import GHC.Word
import NumHask.Array (Array)
import Optics.Core
import Poker.Card (Hole (..), Suit (..), allSuits)
import Poker.Card qualified as C
import Poker.Card.Storable hiding (apply)
import Poker.HandRank
import Poker.Random
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text
import System.Random
import Text.Read (readMaybe)
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Foldable (toList)
-- >>> import Control.Monad.State.Lazy
-- >>> import Data.Bool
-- >>> import Data.Functor.Rep
-- >>> import Optics.Core
-- >>> import Poker.Card
-- >>> import Poker.HandRank
-- >>> import Poker.Random
-- >>> import Poker.Range
-- >>> import Poker.Table
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import System.Random
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Text as Text
-- >>> hvs <- hvs7
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> pretty cs
-- Js2h9s6s8c5sQh5c6c
--
-- >>> t = makeTable defaultTableConfig cs
--
-- >>> pretty t
-- Js2h 9s6s|8c5sQh|5c|6c,hero: 0,o o,9.5 9.0,0.50 1.0,0.0,
-- >>> (Just m) <- readSomeRanges
-- >>> Map.keys m
-- ["any2","count","o2","o9"]
--
-- >>> s = m Map.! "o2"

-- |
-- A 'StartingHand' is the 'Suit'-normalised representation of a
-- poker 'Hole'. For example, the 'Hole' "King of Diamonds, 5 of Hearts" is often referred
-- to as "King-5 offsuit".
--
-- https://en.wikipedia.org/wiki/Texas_hold_%27em_starting_hands
data StartingHand = Paired !C.Rank | Offsuited !C.Rank !C.Rank | Suited !C.Rank !C.Rank
  deriving (Eq, Ord, Show)

instance Pretty StartingHand where
  pretty (Offsuited r1 r2) = pretty r1 <> pretty r2 <> "o"
  pretty (Suited r1 r2) = pretty r1 <> pretty r2 <> "s"
  pretty (Paired r) = pretty r <> pretty r <> "p"

-- | Storable, Indexable version of StartingHand
newtype StartingHandS = StartingHandS {unStartingHandS :: Word8} deriving (Eq, Show, Ord)

instance Pretty StartingHandS where
  pretty s = pretty (review startingHandI s)

-- | Isomorphism between startingHand and startingHandS
startingHandI :: Iso' StartingHand StartingHandS
startingHandI =
  iso fromStartingHand toStartingHand

fromStartingHand :: StartingHand -> StartingHandS
fromStartingHand (Offsuited r0 r1) =
  StartingHandS $ unwrapRankS (view (re rankI) r1) * 13 + unwrapRankS (view (re rankI) r0)
fromStartingHand (Paired p) =
  StartingHandS $ let p' = unwrapRankS (view (re rankI) p) in p' * 13 + p'
fromStartingHand (Suited r0 r1) =
  StartingHandS $ unwrapRankS (view (re rankI) r0) * 13 + unwrapRankS (view (re rankI) r1)

toStartingHand :: StartingHandS -> StartingHand
toStartingHand (StartingHandS x) = case compare d m of
  EQ -> Paired $ view rankI . RankS $ d
  LT -> Offsuited (view rankI . RankS $ m) (view rankI . RankS $ d)
  GT -> Suited (view rankI . RankS $ d) (view rankI . RankS $ m)
  where
    (d, m) = x `divMod` 13

-- | convert from a 'Hole' to a 'StartingHand'
--
-- The base mechanism of this transformation is to forget suit details.
--
-- >>> fromHole (Hole (Card Ace Hearts) (Card Ace Spades))
-- Paired Ace
--
-- Unpaired cards are forced to high low order.
--
-- >>> fromHole (Hole (Card Two Hearts) (Card Ace Spades))
-- Offsuited Ace Two
fromHole :: Hole -> StartingHand
fromHole (Hole (C.Card r s) (C.Card r' s'))
  | r == r' = Paired r
  | s == s' = Suited (max r r') (min r r')
  | otherwise = Offsuited (max r r') (min r r')

fromHole' :: Hole -> StartingHand
fromHole' (Hole (C.Card r s) (C.Card r' s'))
  | r == r' = Paired r
  | s == s' && r > r' = Suited r r'
  | s == s' && r < r' = Suited r' r
  | r > r' = Offsuited r r'
  | otherwise = Offsuited r' r

fromHole'' :: Hole -> StartingHand
fromHole'' (Hole (C.Card r s) (C.Card r' s')) =
  case compare r r' of
    EQ -> Paired r
    GT -> mk r r'
    LT -> mk r' r
  where
    mk = if s == s' then Suited else Offsuited

-- | Enumeration of the iso-equivalence classes of 'Hole' that a 'StartingHand' represents.
--
-- >>> pretty $ toHoles (Paired Ace)
-- [AcAd, AcAh, AcAs, AdAc, AdAh, AdAs, AhAc, AhAd, AhAs, AsAc, AsAd, AsAh]
toHoles :: StartingHand -> [Hole]
toHoles (Paired r) =
  (\(x, y) -> Hole (C.Card r x) (C.Card r y)) <$> enum2 allSuits
toHoles (Suited r0 r1) =
  ((\x -> Hole (C.Card r0 x) (C.Card r1 x)) <$> allSuits)
    <> ((\x -> Hole (C.Card r1 x) (C.Card r0 x)) <$> allSuits)
toHoles (Offsuited r0 r1) =
  ((\(x, y) -> Hole (C.Card r0 x) (C.Card r1 y)) <$> enum2 allSuits)
    <> ((\(x, y) -> Hole (C.Card r0 y) (C.Card r1 x)) <$> enum2 allSuits)

-- | a representative pair of cards for a B, arbitrarily choosing Clubs and Diamonds.
--
-- >>> pretty $ toRepHole (Offsuited Ace Two)
-- Ac2d
toRepHole :: StartingHand -> Hole
toRepHole (Paired r) = Hole (C.Card r Clubs) (C.Card r Diamonds)
toRepHole (Suited r0 r1) = Hole (C.Card r0 Clubs) (C.Card r1 Clubs)
toRepHole (Offsuited r0 r1) = Hole (C.Card r0 Clubs) (C.Card r1 Diamonds)

-- | Convert a StartingHand based on it's type (Offsuited, Pair or Suited).
--
-- >>> fromOPS ("Offsuited","Paired","Suited") <$> [Offsuited Ace King, Paired Ace, Suited Ace King]
-- ["Offsuited","Paired","Suited"]
fromOPS :: (a, a, a) -> StartingHand -> a
fromOPS (a, _, _) (Offsuited _ _) = a
fromOPS (_, a, _) (Paired _) = a
fromOPS (_, _, a) (Suited _ _) = a

-- | A Range is an array with size 169 representing the set of StartingHands.
--
-- Ranged hands can be used in many different contexts. One example is as a statistic across the set of hands. Here is a chart of the chances of winning given a Hole, against another player with 'any2'.
--
-- ![bwin example](other/bwin.svg)
--
-- Another example is as a strategy for a seat: what betting action should be taken, given what I might be holding in my Hole. Always Call looks like:
--
-- >>> :t always RawCall
-- always RawCall :: Range RawAction
--
-- Or the dual to this question: given the betting action that has occurred, what are my guesses about other player strategies.
--
-- Range instances for representable functors tend to be very useful.
--
-- The basic ordering of the underlying vector is:
-- >>> pretty <$> toList (tabulate id :: Range StartingHandS)
-- [22p,32o,42o,52o,62o,72o,82o,92o,T2o,J2o,Q2o,K2o,A2o,32s,33p,43o,53o,63o,73o,83o,93o,T3o,J3o,Q3o,K3o,A3o,42s,43s,44p,54o,64o,74o,84o,94o,T4o,J4o,Q4o,K4o,A4o,52s,53s,54s,55p,65o,75o,85o,95o,T5o,J5o,Q5o,K5o,A5o,62s,63s,64s,65s,66p,76o,86o,96o,T6o,J6o,Q6o,K6o,A6o,72s,73s,74s,75s,76s,77p,87o,97o,T7o,J7o,Q7o,K7o,A7o,82s,83s,84s,85s,86s,87s,88p,98o,T8o,J8o,Q8o,K8o,A8o,92s,93s,94s,95s,96s,97s,98s,99p,T9o,J9o,Q9o,K9o,A9o,T2s,T3s,T4s,T5s,T6s,T7s,T8s,T9s,TTp,JTo,QTo,KTo,ATo,J2s,J3s,J4s,J5s,J6s,J7s,J8s,J9s,JTs,JJp,QJo,KJo,AJo,Q2s,Q3s,Q4s,Q5s,Q6s,Q7s,Q8s,Q9s,QTs,QJs,QQp,KQo,AQo,K2s,K3s,K4s,K5s,K6s,K7s,K8s,K9s,KTs,KJs,KQs,KKp,AKo,A2s,A3s,A4s,A5s,A6s,A7s,A8s,A9s,ATs,AJs,AQs,AKs,AAp]
--
-- The pretty instances, for Text and Char, in contrast, assume a grid pattern eg
--
-- >>> pretty rhText
-- AAp AKs AQs AJs ATs A9s A8s A7s A6s A5s A4s A3s A2s
-- AKo KKp KQs KJs KTs K9s K8s K7s K6s K5s K4s K3s K2s
-- AQo KQo QQp QJs QTs Q9s Q8s Q7s Q6s Q5s Q4s Q3s Q2s
-- AJo KJo QJo JJp JTs J9s J8s J7s J6s J5s J4s J3s J2s
-- ATo KTo QTo JTo TTp T9s T8s T7s T6s T5s T4s T3s T2s
-- A9o K9o Q9o J9o T9o 99p 98s 97s 96s 95s 94s 93s 92s
-- A8o K8o Q8o J8o T8o 98o 88p 87s 86s 85s 84s 83s 82s
-- A7o K7o Q7o J7o T7o 97o 87o 77p 76s 75s 74s 73s 72s
-- A6o K6o Q6o J6o T6o 96o 86o 76o 66p 65s 64s 63s 62s
-- A5o K5o Q5o J5o T5o 95o 85o 75o 65o 55p 54s 53s 52s
-- A4o K4o Q4o J4o T4o 94o 84o 74o 64o 54o 44p 43s 42s
-- A3o K3o Q3o J3o T3o 93o 83o 73o 63o 53o 43o 33p 32s
-- A2o K2o Q2o J2o T2o 92o 82o 72o 62o 52o 42o 32o 22p
--
-- This is a standard convention, with the upper triangle representing suited hands, the lower triangle offsuits, and the diagonal paired hands.
--
-- ![text example](other/text.svg)
newtype Range a = Range
  { array :: Array '[169] a
  }
  deriving (Eq, Foldable)

instance (Show a) => Show (Range a) where
  show (Range a) = show (toList a)

instance (Read a) => Read (Range a) where
  readPrec = Range . fromList <$> readPrec

instance Functor Range where
  fmap f (Range a) = Range (fmap f a)

instance Applicative Range where
  pure a = tabulate (const a)
  (<*>) f a = tabulate (\i -> index f i (index a i))

instance Data.Distributive.Distributive Range where
  distribute = distributeRep

instance Representable Range where
  type Rep Range = StartingHandS

  tabulate f = Range $ tabulate (f . StartingHandS . toEnum . fromMaybe 0 . listToMaybe)

  index (Range a) = index a . (: []) . fromEnum . unStartingHandS

-- | Create a list of lists representing the default textual grid for representing a Range
--
-- See 'rhText' for an example of this.
toGridTextual :: Range a -> [[a]]
toGridTextual s = (\x -> (\y -> index s (StartingHandS $ 13 * (12 - x) + (12 - y))) <$> [0 .. 12]) <$> [0 .. 12]

instance Pretty (Range Text) where
  pretty s = vsep $ hsep . fmap pretty <$> toGridTextual s

instance Pretty (Range Char) where
  pretty s = vsep $ sep . fmap pretty <$> toGridTextual s

-- | on-screen grid-style representation of a Range
--
-- >>> pretty rhText
-- AAp AKs AQs AJs ATs A9s A8s A7s A6s A5s A4s A3s A2s
-- AKo KKp KQs KJs KTs K9s K8s K7s K6s K5s K4s K3s K2s
-- AQo KQo QQp QJs QTs Q9s Q8s Q7s Q6s Q5s Q4s Q3s Q2s
-- AJo KJo QJo JJp JTs J9s J8s J7s J6s J5s J4s J3s J2s
-- ATo KTo QTo JTo TTp T9s T8s T7s T6s T5s T4s T3s T2s
-- A9o K9o Q9o J9o T9o 99p 98s 97s 96s 95s 94s 93s 92s
-- A8o K8o Q8o J8o T8o 98o 88p 87s 86s 85s 84s 83s 82s
-- A7o K7o Q7o J7o T7o 97o 87o 77p 76s 75s 74s 73s 72s
-- A6o K6o Q6o J6o T6o 96o 86o 76o 66p 65s 64s 63s 62s
-- A5o K5o Q5o J5o T5o 95o 85o 75o 65o 55p 54s 53s 52s
-- A4o K4o Q4o J4o T4o 94o 84o 74o 64o 54o 44p 43s 42s
-- A3o K3o Q3o J3o T3o 93o 83o 73o 63o 53o 43o 33p 32s
-- A2o K2o Q2o J2o T2o 92o 82o 72o 62o 52o 42o 32o 22p
rhText :: Range Text
rhText = renderStrict . layoutCompact . pretty <$> tabulate toStartingHand

-- | left pad some text
lpad :: Int -> Text -> Text
lpad n t = pack (replicate (n - Text.length t) ' ') <> t

-- | The combinatorial count for each StartingHand
--
-- > fromIntegral <$> enumBs == handTypeCount
--
-- >>>  pretty $ (lpad 2 . Text.pack . show) <$> handTypeCount
-- 12  8  8  8  8  8  8  8  8  8  8  8  8
-- 24 12  8  8  8  8  8  8  8  8  8  8  8
-- 24 24 12  8  8  8  8  8  8  8  8  8  8
-- 24 24 24 12  8  8  8  8  8  8  8  8  8
-- 24 24 24 24 12  8  8  8  8  8  8  8  8
-- 24 24 24 24 24 12  8  8  8  8  8  8  8
-- 24 24 24 24 24 24 12  8  8  8  8  8  8
-- 24 24 24 24 24 24 24 12  8  8  8  8  8
-- 24 24 24 24 24 24 24 24 12  8  8  8  8
-- 24 24 24 24 24 24 24 24 24 12  8  8  8
-- 24 24 24 24 24 24 24 24 24 24 12  8  8
-- 24 24 24 24 24 24 24 24 24 24 24 12  8
-- 24 24 24 24 24 24 24 24 24 24 24 24 12
handTypeCount :: Range Int
handTypeCount = tabulate $ fromOPS (24, 12, 8) . view (re startingHandI)

-- | A Range with no information about the Hole.
--
-- >>> sum (array any2)
-- 0.9999999999999986
any2 :: Range Double
any2 = let xs = fromIntegral <$> handTypeCount in (/ sum xs) <$> xs

-- | constant value
always :: a -> Range a
always a = tabulate (const a)

-- | Convert from magnitude to order
ordered :: Range Double -> Range Int
ordered r = Range $ fromList $ fmap fst $ List.sortOn snd $ zip [0 ..] (fmap fst $ List.sortOn snd $ zip [0 ..] (toList r))

-- | Create a Range base on Offsuited, Suited or Paired status
--
-- >>> pretty $ ops ('o','p','s')
-- p s s s s s s s s s s s s
-- o p s s s s s s s s s s s
-- o o p s s s s s s s s s s
-- o o o p s s s s s s s s s
-- o o o o p s s s s s s s s
-- o o o o o p s s s s s s s
-- o o o o o o p s s s s s s
-- o o o o o o o p s s s s s
-- o o o o o o o o p s s s s
-- o o o o o o o o o p s s s
-- o o o o o o o o o o p s s
-- o o o o o o o o o o o p s
-- o o o o o o o o o o o o p
ops :: (a, a, a) -> Range a
ops a = tabulate $ fromOPS a . view (re startingHandI)

-- | Raise to the cursor's stack size.
allin :: Table -> Range RawAction
allin ts = tabulate (const (RawRaise x))
  where
    x = stacks ts List.!! fromMaybe 0 (cursor ts)

-- | Given a 'StartingHand', what is the chance of that player winning against p other players, simulated n times.
--
-- >>> simStartingHandWin hvs (Paired Ace) 2 1000
-- 0.85
--
-- >>> simStartingHandWin hvs (Offsuited Three Two) 2 1000
-- 0.3145
simStartingHandWin :: S.Vector Word16 -> StartingHand -> Int -> Int -> Double
simStartingHandWin s b p n =
  (/ fromIntegral n) $ sum $ (\x -> bool (0 :: Double) (1 / fromIntegral (length x)) (0 `elem` x)) . bestLiveHole s <$> tablesB p b 0 n

-- | Win odds
--
-- Takes about a minute:
--
-- > winOdds 2 1000
--
-- ![odds2 example](other/odds2.svg)
--
-- > winOdds 9 1000
--
-- ![odds9 example](other/odds9.svg)
winOdds :: S.Vector Word16 -> Int -> Int -> Range Double
winOdds s p n = tabulate (\b -> simStartingHandWin s (view (re startingHandI) b) p n)

-- | Top x percent of hands, order determined by a Range Double.
--
-- >>> pretty (bool "." "x" <$> topBs (m Map.! "o2") 0.25 :: Range Text.Text)
-- x x x x x x x x x x x x x
-- x x x x x x x x . . . . .
-- x x x x x x . . . . . . .
-- x x x x x . . . . . . . .
-- x x x . x . . . . . . . .
-- x x . . . x . . . . . . .
-- x . . . . . x . . . . . .
-- x . . . . . . x . . . . .
-- x . . . . . . . x . . . .
-- x . . . . . . . . x . . .
-- . . . . . . . . . . . . .
-- . . . . . . . . . . . . .
-- . . . . . . . . . . . . .
topBs :: Range Double -> Double -> Range Bool
topBs bs x = tabulate (`elem` top)
  where
    sortedList x = second snd <$> sortOn (Down . fst . snd) (toList (liftR2 (,) (tabulate id) (liftR2 (,) x (fromIntegral <$> handTypeCount))))
    (total, as) = second reverse $ foldl' (\(c', xs) (b, c) -> (c' + c, (b, c' + c) : xs)) (0, []) (sortedList bs)
    cut = x * total
    top = fst <$> List.takeWhile ((< cut) . snd) as

-- | Construct a Range RawAction that chooses to (Raise r) for the top x% of hands, or Call for the top y%, and thus Fold for the bottom (1-y)%.
--
-- eg raising with your top 10% and calling with your top 50% (top defined by o2 stats) is
--
-- >>> pretty $ fromRawAction <$> rcf (m Map.! "o2") 10 0.1 0.5
-- r r r r r r c c c c c c c
-- r r r r c c c c c c c c c
-- r c r c c c c c c c c c c
-- r c c r c c c c c c f f f
-- r c c c r c c c f f f f f
-- c c c c c r c f f f f f f
-- c c c c f f r f f f f f f
-- c c c f f f f r f f f f f
-- c c c f f f f f r f f f f
-- c c c f f f f f f c f f f
-- c c f f f f f f f f c f f
-- c c f f f f f f f f f c f
-- c c f f f f f f f f f f c
rcf :: Range Double -> Double -> Double -> Double -> Range RawAction
rcf s r x y =
  tabulate
    ( \b ->
        bool
          (bool RawFold RawCall (index (topBs s y) b))
          (RawRaise r)
          (index (topBs s x) b)
    )

-- | Simulate the expected value of a list of ranged hand actions (for seat 0)
--
-- >>> ev hvs 2 100 [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- Just (-0.6099999999999994)
ev :: S.Vector Word16 -> Int -> Int -> [Range RawAction] -> Maybe Double
ev s n sims acts =
  listToMaybe $
    fmap ((+ negate 10) . (/ fromIntegral sims) . sum) $
      List.transpose $
        evs s n sims acts

-- | Simulate winnings for each seat.
--
-- >>> all (==20.0) $ sum <$> evs hvs 2 100 [rcf s 1 0 0.9, rcf s 1 0.3 0.9, rcf s 1 0.1 0.5, rcf s 1 0.6 0.8]
-- True
evs :: S.Vector Word16 -> Int -> Int -> [Range RawAction] -> [[Double]]
evs s n sims acts = fmap (toList . stacks) (evTables s n sims acts)

-- | Simulate end state of tables given strategies.
evTables :: S.Vector Word16 -> Int -> Int -> [Range RawAction] -> [Table]
evTables s n sims acts =
  showdown s . applies acts <$> tables
  where
    cards = evalState (replicateM sims (dealN (5 + 2 * toEnum n))) (mkStdGen 42)
    tables = makeTable (defaultTableConfig {tableSize = n}) <$> cards

-- | Simulate the expected value of a 2 seat game, given the 5 decision point cuts of headsup flop-only holdem described in 'actOn'.
--
-- The 5 points are:
--
-- - UTG (Raise 10)
--
-- - UTG Call
--
-- - BB (Raise 10) on UTG Call
--
-- - UTG Call on BB (Raise 10)
--
-- - BB Call on UTG Raise
--
-- >>> (Just m) <- readSomeRanges
-- >>> s = m Map.! "o2"
--
-- [0,0,0,0,0] is iso to always Fold
--
-- >>> ev2Ranges hvs s 100 [0,0,0,0,0]
-- Just (-0.5)
--
-- [1,_,1,_] is iso to always Raise
--
-- >>> ev2Ranges hvs s 100 [1,1,1,1,1]
-- Just (-1.6999999999999993)
--
-- [0,1,0,_,_] is iso to always Call
--
-- >>> ev2Ranges hvs s 100 [0,1,0,1,1]
-- Just (-0.16999999999999993)
ev2Ranges :: S.Vector Word16 -> Range Double -> Int -> [Double] -> Maybe Double
ev2Ranges s' s sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev s' 2 sims [rcf s 10 s0r s0c, rcf s 10 s1r 1, rcf s 10 0 s2r, rcf s 10 0 s3r]
ev2Ranges _ _ _ _ = Nothing

-- | Apply a 'Range' 'RawAction', supplying a RawAction for the cursor.
--
-- >>> apply (always RawCall) t
-- RawCall
apply :: Range RawAction -> Table -> RawAction
apply s t = fromMaybe RawFold $ case cursor t of
  Nothing -> error "bad juju"
  Just i -> Just $ index s (view startingHandI $ fromHole (playerCards (cards t) List.!! i))

-- | Apply a list of 'Range' 'RawAction's, until betting is closed, or the list ends.
--
-- >>> pretty $ applies (always (RawRaise 10) : (replicate 3 (always RawCall))) t
-- Js2h 9s6s|8c5sQh|5c|6c,hero: ,c c,0.0 0.0,10 10,0.0,c1:9.0r0
applies :: [Range RawAction] -> Table -> Table
applies ss t = go ss t
  where
    go [] t = t
    go (s : ss') t =
      bool (applies ss' (actOn (apply s t) t)) t (closed t)

-- | deal n cards given a Hole has been dealt.
--
-- >>> pretty $ evalState (dealStartingHand (Paired Ace) 7) (mkStdGen 42)
-- Jh2h9h6h7s5hQd
dealStartingHand :: (RandomGen g) => StartingHand -> Int -> State g CardsS
dealStartingHand b n =
  dealNWith (toEnum n)
    . ( \(x, y) ->
          let (x', y') =
                bool (y, x) (x, y) (x <= y)
           in S.splitAt x' (unwrapCardsS allCardsS)
                & second (S.splitAt (y' - x'))
                & (\(t, (t', t'')) -> t <> S.tail t' <> S.tail t'')
                & CardsS
      )
    . (\(Hole x y) -> (fromIntegral (unwrapCardS (review cardI x)), fromIntegral (unwrapCardS (review cardI y))))
    . toRepHole
    $ b

-- | deal a table given player i has been dealt a B
--
-- >>> pretty $ evalState (dealTableHole defaultTableConfig 0 (Paired Ace)) (mkStdGen 42)
-- AcAd Jh2h|9h6h7s|5h|Qd,hero: 0,o o,9.5 9.0,0.50 1.0,0.0,
dealTableHole :: (RandomGen g) => TableConfig -> Int -> StartingHand -> State g Table
dealTableHole cfg i b = do
  (CardsS xs) <- dealStartingHand b (5 + (tableSize cfg - 1) * 2)
  pure $
    makeTable cfg $
      CardsS $
        S.take (2 * i) xs
          <> (\(Hole x y) -> unwrapCardsS $ review cardsI [x, y]) (toRepHole b)
          <> S.drop (2 * i) xs

-- | create a list of n dealt tables, with p players, where b is dealt to player k
--
-- >>> pretty $ tablesB 2 (Paired Ace) 1 3
-- [ Jh2h AcAd|9h6h7s|5h|Qd,hero: 0,o o,9.5 9.0,0.50 1.0,0.0,
-- , 5d5s AcAd|8h9c8s|Kc|9h,hero: 0,o o,9.5 9.0,0.50 1.0,0.0,
-- , Jd5c AcAd|ThAs3s|8c|9h,hero: 0,o o,9.5 9.0,0.50 1.0,0.0, ]
tablesB :: Int -> StartingHand -> Int -> Int -> [Table]
tablesB p b k n =
  evalState
    ( replicateM
        n
        (dealTableHole (defaultTableConfig {tableSize = p}) k b)
    )
    (mkStdGen 42)

-- | create some common simulation results
someRanges :: S.Vector Word16 -> Int -> Map.Map Text (Range Double)
someRanges s n = do
  Map.fromList
    [ ("o2", tabulate (\b -> simStartingHandWin s (view (re startingHandI) b) 2 n)),
      ("o9", tabulate (\b -> simStartingHandWin s (view (re startingHandI) b) 9 n)),
      ("count", fromIntegral <$> handTypeCount),
      ("any2", any2)
    ]

-- | write some common simulation results to file.
--
-- > writeSomeRanges 1000
--
-- n = 100000 is about 5 mins
writeSomeRanges :: Int -> IO ()
writeSomeRanges n = do
  s <- hvs7
  writeFile "other/some.str" (show $ someRanges s n)

-- | read Range map
--
-- >>> (Just m) <- readSomeRanges
-- >>> index (m Map.! "o2") (view startingHandI $ Suited Jack Ten)
-- 0.574275
readSomeRanges :: IO (Maybe (Map.Map Text (Range Double)))
readSomeRanges = do
  t <- readFile "other/some.str"
  pure $ readMaybe t
