{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | __A Shaped Hole__
--
-- A 'Suit' in holdem is unordered; no Suit is better or worse than another when assessing hand ranks. A consequence of the unordered nature of suits is that the set of all 'Hole's, the pair of cards dealt to a player, can be divided into iso-equivalence classes where equivalance is seen from the point of view of hand strength or value in the presence of non-determinism.
--
-- For example, there is no difference pre-flop between being dealt a @AhKc@ and a @AsKd@. Specifically, the probability of forming various hands by the river is exactly the same for both hands. Another example is @AhKc@ and @KcAh@ as the order of the cards doesn't effect the potential hand value.
--
-- It follows then that the Strategy (how should one progress or act in a holdem game) for both of these example 'Hole's is also identical. This is the idea behind a 'ShapedHole'.
--
-- A 'ShapedHole' can be:
--
-- - a 'Pair' (two 'Card's of the same 'Rank'): which occurs 12 times in a full enumerations of a fresh deck. (4 suits x 3 suits)
--
-- - an 'Offsuit' (two 'Card's of different 'Rank', with the first rank of a 'ShapedHole' representing the highest rank): which occurs 24 time ((low ranked dealt first && high rank dealt first) x 4 suits x 3 suits)
--
-- - 'Suited' (two 'Card's of different 'Rank' and the same 'Suit'): which occurs 8 times ((low && high ranked) x 4 suits)
--
-- ![count example](other/count.svg)
--
-- With respect to poker strategy (how should I act), a 'ShapedHole' can be considered to be a lossless compression algorithm. Transformations from 'Hole' to 'ShapedHole' reduce the necessary enumeration of Strategy from 2652 different possibilities to 169, with no loss of fidelity.
--
--
-- __A Ranged Hole__
--
-- A 'RangedHole' a is a vector of a's representing a value for each possible 'ShapedHole' (there are 169 of them). Poker strategy (what should I do) is often expressed in terms of 'ShapedHole's. Some examples:
--
-- - "In the big blind, facing an AllIn raise, you should call with your best 20% of hands."
--
-- This can be represented as a RangedHole Bool. "Pair Ace" is likely to be True and "Offsuit Three Two" False.
--
-- - "I think that bet is a complete bluff"
-- A RangedHole Double representing a probability distribution eg 0 for good ShapedHoles (not bluffs) and a small value for all other shaped hands (bluffs), adding to one.
--
-- - "I always lose with Jacks."
--
-- A statement that can be verified by examining a hand history of the player and dividing up profit and loss into starting hand HoleRanges.
--
module Poker.RangedHole
  ( -- * Storable ShapedHole
    ShapedHoleS (..),
    shapedHoleS,
    fromHole,
    fromHole',
    fromHole'',
    toHoles,
    toRepHole,
    fromOPS,

    -- * RangedHole
    RangedHole (..),
    rhText,
    enumShapedHoles,
    handTypeCount,
    any2,
    always,
    allin,
    ordered,

    -- * Simulation
    winHole,
    winOdds,
    topBs,
    ev,
    evs,
    ev2Ranges,
    rcf,

    -- * Table Interaction
    dealShapedHole,
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
  )
where

import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as S
import GHC.Exts hiding (toList)
import GHC.Read
import GHC.Word
import NumHask.Array (Array)
import Poker hiding (allCards, fromList)
import Poker.Card.Storable hiding (apply)
import Poker.Random
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Random
import Text.Read (readMaybe)
import Prelude
import Optics.Core
import Poker.HandRank.List (cardsI, rankI, cardI)
import Poker.Cards (allCards)

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Control.Monad.State.Lazy
-- >>> import Data.Bool
-- >>> import Data.Functor.Rep
-- >>> import Lens.Micro
-- >>> import Poker
-- >>> import Poker.Card.Storable
-- >>> import Poker.Random
-- >>> import Poker.RangedHole
-- >>> import Poker.Table
-- >>> import Prelude
-- >>> import Prettyprinter
-- >>> import System.Random
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Text as Text
-- >>> cs = evalState (dealN 9) (mkStdGen 42)
-- >>> pretty cs
-- [Ac, 7s, Tc, 5s, 6d, 7c, 6s, 9c, 4s]
--
-- >>> t = makeTable defaultTableConfig cs
-- >>> pretty t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
--
-- >>> (Just m) <- readSomeRanges
-- >>> Map.keys m
-- ["count","freq","o2","o9"]
--
-- >>> s = m Map.! "o2"

-- | Storable, Indexable version of ShapedHole
newtype ShapedHoleS = ShapedHoleS {unShapedHoleS :: Word8} deriving (Eq, Show, Ord)

instance Pretty ShapedHoleS where
  pretty = pretty . view (re shapedHoleS)

-- | Isomorphism between shapedHole and shapedHoleS
shapedHoleS :: Iso' ShapedHole ShapedHoleS
shapedHoleS =
  iso fromShapedHole toShapedHole

fromShapedHole :: ShapedHole -> ShapedHoleS
fromShapedHole (MkOffsuit r0 r1) =
  ShapedHoleS $ unwrapRank (view (re rankI) r1) * 13 + unwrapRank (view (re rankI) r0)
fromShapedHole (MkPair p) =
  ShapedHoleS $ let p' = unwrapRank (view (re rankI) p) in p' * 13 + p'
fromShapedHole (MkSuited r0 r1) =
  ShapedHoleS $ unwrapRank (view (re rankI) r0) * 13 + unwrapRank (view (re rankI) r1)

toShapedHole :: ShapedHoleS -> ShapedHole
toShapedHole (ShapedHoleS x) = case compare d m of
  EQ -> MkPair $ view rankI . Rank $ d
  LT -> MkOffsuit (view rankI . Rank $ m) (view rankI . Rank $ d)
  GT -> MkSuited (view rankI . Rank $ d) (view rankI . Rank $ m)
  where
    (d, m) = x `divMod` 13

-- | convert from a 'Hole' to a 'ShapedHole'
--
-- The base mechanism of this transformation is to forget suit details.
--
-- >>> fromHole (MkHole (Card Ace Heart) (Card Ace Spade))
-- MkPair Ace
--
-- Unpaired cards are forced to high low order.
--
-- >>> fromHole (MkHole (Card Two Heart) (Card Ace Spade))
-- MkOffsuit Ace Two
fromHole :: Hole -> ShapedHole
fromHole (Hole (Poker.Card r s) (Poker.Card r' s'))
  | r == r' = MkPair r
  | s == s' = MkSuited (max r r') (min r r')
  | otherwise = MkOffsuit (max r r') (min r r')

fromHole' :: Hole -> ShapedHole
fromHole' (Hole (Poker.Card r s) (Poker.Card r' s'))
  | r == r' = MkPair r
  | s == s' && r > r' = MkSuited r r'
  | s == s' && r < r' = MkSuited r' r
  | r > r' = MkOffsuit r r'
  | otherwise = MkOffsuit r' r

fromHole'' :: Hole -> ShapedHole
fromHole'' (Hole (Poker.Card r s) (Poker.Card r' s')) =
  case compare r r' of
    EQ -> MkPair r
    GT -> mk r r'
    LT -> mk r' r
  where mk = if s == s' then MkSuited else MkOffsuit

-- | Enumeration of the iso-equivalence classes of 'Hole' that a 'ShapedHole' represents.
--
-- >>> pretty $ toHoles (MkPair Ace)
-- [AcAd, AcAh, AcAs, AdAc, AdAh, AdAs, AhAc, AhAd, AhAs, AsAc, AsAd, AsAh]
toHoles :: ShapedHole -> [Hole]
toHoles (MkPair r) = (\(x, y) -> MkHole (Poker.Card r x) (Poker.Card r y)) <$> enum2 allSuits
toHoles (MkSuited r0 r1) =
  ((\x -> MkHole (Poker.Card r0 x) (Poker.Card r1 x)) <$> allSuits)
    <> ((\x -> MkHole (Poker.Card r1 x) (Poker.Card r0 x)) <$> allSuits)
toHoles (MkOffsuit r0 r1) =
  ((\(x, y) -> MkHole (Poker.Card r0 x) (Poker.Card r1 y)) <$> enum2 allSuits)
    <> ((\(x, y) -> MkHole (Poker.Card r0 y) (Poker.Card r1 x)) <$> enum2 allSuits)

-- | a representative pair of cards for a B, arbitrarily choosing Club and Diamond.
--
-- >>> pretty $ toRepHole (MkOffsuit Ace Two)
-- Ac2d
toRepHole :: ShapedHole -> Hole
toRepHole (MkPair r) = MkHole (Poker.Card r Club) (Poker.Card r Diamond)
toRepHole (MkSuited r0 r1) = MkHole (Poker.Card r0 Club) (Poker.Card r1 Club)
toRepHole (MkOffsuit r0 r1) = MkHole (Poker.Card r0 Club) (Poker.Card r1 Diamond)

-- | Convert a ShapedHole based on it's type (Offsuit, Pair or Suited).
--
-- >>> fromOPS ("Offsuit","Pair","Suited") <$> [MkOffsuit Ace King, MkPair Ace, MkSuited Ace King
fromOPS :: (a, a, a) -> ShapedHole -> a
fromOPS (a, _, _) (MkOffsuit _ _) = a
fromOPS (_, a, _) (MkPair _) = a
fromOPS (_, _, a) (MkSuited _ _) = a

-- | A RangedHole is an array with size 169 representing the set of ShapedHoles.
--
-- Ranged hands can be used in many different contexts. One example is as a statistic across the set of hands. Here is a chart of the chances of winning given a Hole, against another player with 'any2'.
--
-- ![bwin example](other/bwin.svg)
--
-- Another example is as a strategy for a seat: what betting action should be taken, given what I might be holding in my Hole. Always Call looks like:
--
-- >>> :t always RawCall
-- always RawCall :: RangedHole RawAction
--
-- Or the dual to this question: given the betting action that has occurred, what are my guesses about other player strategies. (FIXME: NYI)
--
-- RangedHole instances for representable functors tend to be very useful.
--
-- The basic ordering of the underlying vector is:
-- >>> pretty <$> toList (tabulate id :: RangedHole ShapedHoleS)
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
newtype RangedHole a = RangedHole
  { array :: Array '[169] a
  }
  deriving (Eq, Foldable)

instance (Show a) => Show (RangedHole a) where
  show (RangedHole a) = show (toList a)

instance (Read a) => Read (RangedHole a) where
  readPrec = RangedHole . fromList <$> readPrec

instance Functor RangedHole where
  fmap f (RangedHole a) = RangedHole (fmap f a)

instance Applicative RangedHole where
  pure a = tabulate (const a)
  (<*>) f a = tabulate (\i -> index f i (index a i))

instance Data.Distributive.Distributive RangedHole where
  distribute = distributeRep

instance Representable RangedHole where
  type Rep RangedHole = ShapedHoleS

  tabulate f = RangedHole $ tabulate (f . ShapedHoleS . toEnum . fromMaybe 0 . listToMaybe)

  index (RangedHole a) = index a . (: []) . fromEnum . unShapedHoleS

-- | Create a list of lists representing the default textual grid for representing a RangedHole
--
-- See 'rhText' for an example of this.
toGridTextual :: RangedHole a -> [[a]]
toGridTextual s = (\x -> (\y -> index s (ShapedHoleS $ 13 * (12 - x) + (12 - y))) <$> [0 .. 12] ) <$> [0 .. 12]

instance Pretty (RangedHole Text) where
  pretty s = vsep $ hsep . fmap pretty <$> toGridTextual s

instance Pretty (RangedHole Char) where
  pretty s = vsep $ sep . fmap pretty <$> toGridTextual s

-- | on-screen grid-style representation of a RangedHole
--
-- >>> pretty rhText
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
rhText :: RangedHole Text
rhText = renderStrict . layoutCompact . pretty <$> tabulate toShapedHole

-- | left pad some text
lpad :: Int -> Text -> Text
lpad n t = pack (replicate (n - Text.length t) ' ') <> t

-- | enumerate card pairs from a fresh deck and count the ShapedHoles
--
-- handTypeCount == enumShapedHoles
--
enumShapedHoles :: RangedHole Int
enumShapedHoles =
  tabulate
    ( Map.fromListWith
        (+)
        ( (,1) . view shapedHoleS . fromHole . uncurry MkHole
            <$> enum2 Poker.Cards.allCards
        )
        Map.!
    )

-- | The combinatorial count for each ShapedHole
--
-- > fromIntegral <$> enumBs == handTypeCount
--
-- >>>  pretty $ (lpad 2 . pack . show) <$> handTypeCount
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
handTypeCount :: RangedHole Int
handTypeCount = tabulate $ fromOPS (24, 12, 8) . view (re shapedHoleS)

-- | A RangedHole with no information about the Hole.
--
-- >>> sum (array any2)
-- 0.9999999999999986
any2 :: RangedHole Double
any2 = let xs = fromIntegral <$> handTypeCount in (/ sum xs) <$> xs

-- | constant value
always :: a -> RangedHole a
always a = tabulate (const a)

-- | Convert from magnitude to order
ordered :: RangedHole Double -> RangedHole Int
ordered r = RangedHole $ fromList $ fmap fst $ List.sortOn snd $ zip [0..] (fmap fst $ List.sortOn snd $ zip [0..] (toList r))

-- | Raise to the cursor's stack size.
allin :: Table -> RangedHole RawAction
allin ts = tabulate (const (RawRaise x))
  where
    x = stacks ts List.!! fromMaybe 0 (cursor ts)

-- | Given a 'ShapedHole', what is the chance of that player winning against p other players, simulated n times.
--
-- >>> winHole (MkPair Two) 2 1000
-- 0.4995
winHole :: ShapedHole -> Int -> Int -> Double
winHole b p n =
  (/ fromIntegral n) $ sum $ (\x -> bool (0 :: Double) (1 / fromIntegral (length x)) (0 `elem` x)) . bestLiveHole <$> tablesB p b 0 n

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
winOdds :: Int -> Int -> RangedHole Double
winOdds p n = tabulate (\b -> winHole (view (re shapedHoleS) b) p n)

-- | Top x percent of hands, order determined by a RangedHole Double.
--
-- >>> pretty (bool "." "x" <$> topBs (m Map.! "o2") 0.25 :: RangedHole Text.Text)
-- x x x x x x x x x x . . .
-- x x x x x x . . . . . . .
-- x x x x x . . . . . . . .
-- x x x x . . . . . . . . .
-- x x x x x . . . . . . . .
-- x x x . . x . . . . . . .
-- x x . . . . x . . . . . .
-- x x . . . . . x . . . . .
-- x . . . . . . . x . . . .
-- x . . . . . . . . x . . .
-- x . . . . . . . . . . . .
-- x . . . . . . . . . . . .
-- x . . . . . . . . . . . .
topBs :: RangedHole Double -> Double -> RangedHole Bool
topBs bs x = tabulate (`elem` top)
  where
    sortedList x = second snd <$> sortOn (Down . fst . snd) (toList (liftR2 (,) (tabulate id) (liftR2 (,) x (fromIntegral <$> handTypeCount))))
    (total, as) = second reverse $ foldl' (\(c', xs) (b, c) -> (c' + c, (b, c' + c) : xs)) (0, []) (sortedList bs)
    cut = x * total
    top = fst <$> List.takeWhile ((< cut) . snd) as

-- | Construct a RangedHole RawAction that chooses to (Raise r) for the top x% of hands, or Call for the top y%, and thus Fold for the bottom (1-y)%.
--
-- eg raising with your top 10% and calling with your top 50% (top defined by o2 stats) is
--
-- >>> pretty $ fromRawAction <$> rcf (m Map.! "o2") 10 0.1 0.5
-- r r r r r c c c c c c c c
-- r r c c c c c c c c c c c
-- r r r c c c c c c f f f f
-- r r c r c c c f f f f f f
-- r c c c r c c f f f f f f
-- r c c c c r f f f f f f f
-- c c c c c c r f f f f f f
-- c c c c c f f r f f f f f
-- c c c c f f f f r f f f f
-- c c c f f f f f f c f f f
-- c c c f f f f f f f c f f
-- c c c f f f f f f f f c f
-- c c c f f f f f f f f f c
rcf :: RangedHole Double -> Double -> Double -> Double -> RangedHole RawAction
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
-- >>> ev 2 100 [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- Just 2.999999999999936e-2
ev :: Int -> Int -> [RangedHole RawAction] -> Maybe Double
ev n sims acts =
  listToMaybe $
    fmap ((+ negate 10) . (/ fromIntegral sims) . sum) $
      List.transpose $
        evs n sims acts

-- | Simulate winnings for each seat.
--
-- >>> all (==20.0) $ sum <$> evs 2 100 [rcf s 1 0 0.9, rcf s 1 0.3 0.9, rcf s 1 0.1 0.5, rcf s 1 0.6 0.8]
-- True
evs :: Int -> Int -> [RangedHole RawAction] -> [[Double]]
evs n sims acts = fmap (toList . stacks) (evTables n sims acts)

-- | Simulate end state of tables given strategies.
evTables :: Int -> Int -> [RangedHole RawAction] -> [Table]
evTables n sims acts =
  showdown . applies acts <$> tables
  where
    cards = Cards . S.fromList . fmap unwrapCard <$> evalState (replicateM sims (dealN (5 + 2 * toEnum n))) (mkStdGen 42)
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
-- >>> ev2Ranges s 100 [0,0,0,0,0]
-- Just (-0.5)
--
-- [1,_,1,_] is iso to always Raise
--
-- >>> ev2Ranges s 100 [1,1,1,1,1]
-- Just (-1.205)
--
-- [0,1,0,_,_] is iso to always Call
--
-- >>> ev2Ranges s 100 [0,1,0,1,1]
-- Just (-0.125)
ev2Ranges :: RangedHole Double -> Int -> [Double] -> Maybe Double
ev2Ranges s sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev 2 sims [rcf s 10 s0r s0c, rcf s 10 s1r 1, rcf s 10 0 s2r, rcf s 10 0 s3r]
ev2Ranges _ _ _ = Nothing

-- | Apply a 'RangedHole' 'RawAction', supplying a RawAction for the cursor.
--
-- >>> apply (always RawCall) t
-- RawCall
apply :: RangedHole RawAction -> Table -> RawAction
apply s t = fromMaybe RawFold $ case cursor t of
  Nothing -> error "bad juju"
  Just i -> Just $ index s (view shapedHoleS $ fromHole (playerCards (cards t) List.!! i))

-- | Apply a list of 'RangedHole' 'RawAction's, until betting is closed, or the list ends.
--
-- >>> pretty $ applies (always (RawRaise 10) : (replicate 3 (always RawCall))) t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
applies :: [RangedHole RawAction] -> Table -> Table
applies ss t = go ss t
  where
    go [] t = t
    go (s : ss') t =
      bool (applies ss' (actOn (apply s t) t)) t (closed t)

-- | deal n cards given a Hole has been dealt.
--
-- >>> pretty $ evalState (dealShapedHole (MkPair Ace) 7) (mkStdGen 42)
-- Ah7sTc5s6d7c6s
dealShapedHole :: (RandomGen g) => ShapedHole -> Int -> State g Cards
dealShapedHole b n =
  dealNWith (toEnum n)
    . ( \(x, y) ->
          let (x', y') =
                bool (y, x) (x, y) (x <= y)
           in S.splitAt x' (unwrapCards Poker.Card.Storable.allCards)
                & second (S.splitAt (y' - x'))
                & (\(t, (t', t'')) -> t <> S.tail t' <> S.tail t'')
                & Cards
      )
    . (\(Hole x y) -> (fromIntegral (unwrapCard (view cardI x)), fromIntegral (unwrapCard (view cardI y))))
    . toRepHole
    $ b

-- | deal a table given player i has been dealt a B
--
-- >>> pretty $ evalState (dealTableHole defaultTableConfig 0 (MkPair Ace)) (mkStdGen 42)
-- AcAd Ah7s|Tc5s6d|7c|6s,hero: 0,o o,9.5 9,0.5 1,0,
dealTableHole :: (RandomGen g) => TableConfig -> Int -> ShapedHole -> State g Table
dealTableHole cfg i b = do
  (Cards xs) <- dealShapedHole b (5 + (tableSize cfg - 1) * 2)
  pure $
    makeTable cfg $
      Cards $
        S.take (2 * i) xs
          <> (\(Hole x y) -> unwrapCards $ view cardsI [x, y]) (toRepHole b)
          <> S.drop (2 * i) xs

-- | create a list of n dealt tables, with p players, where b is dealt to player k
--
-- >>> pretty $ tablesB 2 (MkPair Ace) 1 3
-- [ Ah7s AcAd|Tc5s6d|7c|6s,hero: 0,o o,9.5 9,0.5 1,0,
-- , 7s4s AcAd|Td3d6c|Kh|Ts,hero: 0,o o,9.5 9,0.5 1,0,
-- , 9c8s AcAd|Ks2h4h|5d|Tc,hero: 0,o o,9.5 9,0.5 1,0, ]
tablesB :: Int -> ShapedHole -> Int -> Int -> [Table]
tablesB p b k n =
  evalState
    ( replicateM
        n
        (dealTableHole (defaultTableConfig {tableSize = p}) k b)
    )
    (mkStdGen 42)

-- | create some common simulation results
someRanges :: Int -> Map.Map Text (RangedHole Double)
someRanges n = do
  Map.fromList
    [ ("o2", tabulate (\b -> winHole (view (re shapedHoleS) b) 2 n)),
      ("o9", tabulate (\b -> winHole (view (re shapedHoleS) b) 9 n)),
      ("count", fromIntegral <$> handTypeCount),
      ("any2", any2)
    ]

-- | write some common simulation results to file.
--
-- > writeSomeRanges 1000
--
-- n = 100000 is about 5 mins
writeSomeRanges :: Int -> IO ()
writeSomeRanges n = writeFile "other/some.str" (show $ someRanges n)

-- | read RangedHole map
-- FIXME:
--
-- >>> (Just m) <- readSomeRanges
-- >>> index (m Map.! "o2") (from shapedHoleS $ MkSuited Jack Ten)
-- 0.5742
readSomeRanges :: IO (Maybe (Map.Map Text (RangedHole Double)))
readSomeRanges = do
  t <- readFile "other/some.str"
  pure $ readMaybe t
