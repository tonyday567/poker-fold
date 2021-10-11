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
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | __A Shaped Hand__
--
-- A 'Suit' in holdem is unordered; no Suit is better or worse than another when assessing hand ranks. A consequence of the unordered nature of suits is that the set of all 'Hand's, the pair of cards dealt to a player, can be divided into iso-equivalence classes where equivalance is seen from the point of view of hand strength or value in the presence of non-determinism.
--
-- For example, there is no difference pre-flop between being dealt a @AhKc@ and a @AsKd@. Specifically, the probability of forming various hands by the river is exactly the same for both hands. Another example is @AhKc@ and @KcAh@ as the order of the cards doesn't effect the potential hand value.
--
-- It follows then that the Strategy (how should one progress or act in a holdem game) for both of these example 'Hand's is also identical. This is the idea behind a 'ShapedHand'.
--
-- A 'ShapedHand' can be:
--
-- - a 'Pair' (two 'Card's of the same 'Rank'): which occurs 12 times in a full enumerations of a fresh deck. (4 suits x 3 suits)
--
-- - an 'Offsuit' (two 'Card's of different 'Rank', with the first rank of a 'ShapedHand' representing the highest rank): which occurs 24 time ((low ranked dealt first && high rank dealt first) x 4 suits x 3 suits)
--
-- - 'Suited' (two 'Card's of different 'Rank' and the same 'Suit'): which occurs 8 times ((low && high ranked) x 4 suits)
--
-- ![count example](other/count.svg)
--
-- With respect to poker strategy (how should I act), a 'ShapedHand' can be considered to be a lossless compression algorithm. Transformations from 'Hand' to 'ShapedHand' reduce the necessary enumeration of Strategy from 2652 different possibilities to 169, with no loss of fidelity.
--
--
-- __A Ranged Hand__
--
-- A 'RangedHand' a is a vector of a's representing a value for each possible 'ShapedHand' (there are 169 of them). Poker strategy (what should I do) is often expressed in terms of 'ShapedHand's. Some examples:
--
-- - "In the big blind, facing an AllIn raise, you should call with your best 20% of hands."
--
-- This can be represented as a RangedHand Bool. "Pair Ace" is likely to be True and "Offsuit Three Two" False.
--
-- - "I think that bet is a complete bluff"
-- A RangedHand Double representing a probability distribution eg 0 for good ShapedHands (not bluffs) and a small value for all other shaped hands (bluffs), adding to one.
--
-- - "I always lose with Jacks."
--
-- A statement that can be verified by examining a hand history of the player and dividing up profit and loss into starting hand HandRanges.
--
module Poker.RangedHand
  ( -- * Storable ShapedHand
    ShapedHandS (..),
    shapedHandS,
    fromHand,
    toHands,
    toRepHand,

    -- * RangedHand
    RangedHand (..),
    stratText,
    enumShapedHands,
    handTypeCount,
    any2,
    always,
    allin,

    -- * Simulation
    winHand,
    winOdds,
    topBs,
    ev,
    evs,
    ev2Ranges,
    rcf,

    -- * Table Interaction
    dealShapedHand,
    dealTableHand,
    tablesB,
    apply,
    applies,

    -- * Saved RangeHands
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
import Data.Function ((&))
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
import Poker hiding (fromList)
import Poker.Card.Storable
import Poker.Random
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Random
import Text.Read (readMaybe)
import Prelude

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
-- >>> import Poker.RangedHand
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

-- |
newtype ShapedHandS = ShapedHandS {unShapedHandS :: Word8} deriving (Eq, Show, Ord)

-- | Isomorphism between shapedHand and shapedHandS
shapedHandS :: Iso ShapedHand ShapedHandS
shapedHandS =
  Iso fromShapedHand toShapedHand

fromShapedHand :: ShapedHand -> ShapedHandS
fromShapedHand (MkOffsuit r0 r1) =
  ShapedHandS $ unwrapRank (from rankS r1) * 13 + unwrapRank (from rankS r0)
fromShapedHand (MkPair p) =
  ShapedHandS $ let p' = unwrapRank (from rankS p) in p' * 13 + p'
fromShapedHand (MkSuited r0 r1) =
  ShapedHandS $ unwrapRank (from rankS r0) * 13 + unwrapRank (from rankS r1)

toShapedHand :: ShapedHandS -> ShapedHand
toShapedHand (ShapedHandS x) = case compare d m of
  EQ -> MkPair $ to rankS . RankS $ d
  LT -> MkOffsuit (to rankS . RankS $ m) (to rankS . RankS $ d)
  GT -> MkSuited (to rankS . RankS $ d) (to rankS . RankS $ m)
  where
    (d, m) = x `divMod` 13

-- | convert from a 'Hand' to a 'ShapedHand'
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

-- | Enumeration of the iso-equivalence classes of 'Hand' that a 'ShapedHand' represents.
--
-- >>> pretty $ toHands (MkPair Ace)
-- [AcAd, AcAh, AcAs, AdAc, AdAh, AdAs, AhAc, AhAd, AhAs, AsAc, AsAd, AsAh]
toHands :: ShapedHand -> [Hand]
toHands (MkPair r) = (\(x, y) -> MkHand (Card r x) (Card r y)) <$> enum2 allSuits
toHands (MkSuited r0 r1) =
  ((\x -> MkHand (Card r0 x) (Card r1 x)) <$> allSuits)
    <> ((\x -> MkHand (Card r1 x) (Card r0 x)) <$> allSuits)
toHands (MkOffsuit r0 r1) =
  ((\(x, y) -> MkHand (Card r0 x) (Card r1 y)) <$> enum2 allSuits)
    <> ((\(x, y) -> MkHand (Card r0 y) (Card r1 x)) <$> enum2 allSuits)

-- | a representative pair of cards for a B, arbitrarily choosing Club and Diamond.
--
-- >>> pretty $ toRepHand (MkOffsuit Ace Two)
-- Ac2d
toRepHand :: ShapedHand -> Hand
toRepHand (MkPair r) = MkHand (Card r Club) (Card r Diamond)
toRepHand (MkSuited r0 r1) = MkHand (Card r0 Club) (Card r1 Club)
toRepHand (MkOffsuit r0 r1) = MkHand (Card r0 Club) (Card r1 Diamond)

-- | A RangedHand is an array with size 169 representing the set of ShapedHands.
--
-- Ranged hands can be used in many different contexts. One example is as a statistic across the set of hands. Here is a chart of the chances of winning given a Hand, against another player with 'any2'.
--
-- ![bwin example](other/bwin.svg)
--
-- Another example is as a strategy for a seat: what betting action should be taken, given what I might be holding in my Hand. Always Call looks like:
--
-- >>> :t always RawCall
-- always RawCall :: RangedHand RawAction
--
-- Or the dual to this question: given the betting action that has occurred, what are my guesses about other player strategies. (FIXME: NYI)
--
-- RangedHand instances for representable functors tend to be very useful.
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

instance Applicative RangedHand where
  pure a = tabulate (const a)
  (<*>) f a = tabulate (\i -> index f i (index a i))

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

-- | on-screen grid-style representation of a RangedHand
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

-- | left pad some text
lpad :: Int -> Text -> Text
lpad n t = pack (replicate (n - Text.length t) ' ') <> t

-- | enumerate card pairs from a fresh deck and count the ShapedHands
--
-- handTypeCount == enumShapedHands
--
enumShapedHands :: RangedHand Int
enumShapedHands =
  tabulate
    ( Map.fromListWith
        (+)
        ( (,1) . from shapedHandS . fromHand . uncurry MkHand
            <$> enum2 allCards
        )
        Map.!
    )

-- | The theoretical combinatorial count.
--
-- > fromIntegral <$> enumBs == handTypeCount
--
-- >>> import Prettyprinter.Render.Text (renderStrict)
-- >>> pretty $ (lpad 4 . renderStrict . layoutCompact . pretty) <$> handTypeCount
--   12   24   24   24   24   24   24   24   24   24   24   24   24
--    8   12   24   24   24   24   24   24   24   24   24   24   24
--    8    8   12   24   24   24   24   24   24   24   24   24   24
--    8    8    8   12   24   24   24   24   24   24   24   24   24
--    8    8    8    8   12   24   24   24   24   24   24   24   24
--    8    8    8    8    8   12   24   24   24   24   24   24   24
--    8    8    8    8    8    8   12   24   24   24   24   24   24
--    8    8    8    8    8    8    8   12   24   24   24   24   24
--    8    8    8    8    8    8    8    8   12   24   24   24   24
--    8    8    8    8    8    8    8    8    8   12   24   24   24
--    8    8    8    8    8    8    8    8    8    8   12   24   24
--    8    8    8    8    8    8    8    8    8    8    8   12   24
--    8    8    8    8    8    8    8    8    8    8    8    8   12
handTypeCount :: RangedHand Int
handTypeCount = tabulate $ \x -> case toShapedHand x of
  (Pair _) -> 12
  (Suited _ _) -> 8
  (Offsuit _ _) -> 24

-- | A RangedHand with no information about the Hand.
--
-- >>> sum (array any2)
-- 0.9999999999999986
any2 :: RangedHand Double
any2 = let xs = fromIntegral <$> handTypeCount in (/ sum xs) <$> xs

-- | Always perform an action
always :: RawAction -> RangedHand RawAction
always a = tabulate (const a)

-- | Raise to the cursor's stack size.
allin :: Table -> RangedHand RawAction
allin ts = tabulate (const (RawRaise x))
  where
    x = stacks ts List.!! fromMaybe 0 (cursor ts)

-- | Given a 'ShapedHand', what is the chance of that player winning against p other players, simulated n times.
--
-- >>> winHand (MkPair Two) 2 1000
-- 0.4995
winHand :: ShapedHand -> Int -> Int -> Double
winHand b p n =
  (/ fromIntegral n) $ sum $ (\x -> bool (0 :: Double) (1 / fromIntegral (length x)) (0 `elem` x)) . bestLiveHand <$> tablesB p b 0 n

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
winOdds :: Int -> Int -> RangedHand Double
winOdds p n = tabulate (\b -> winHand (to shapedHandS b) p n)

-- | Top x percent of hands, order determined by a RangedHand Double.
--
-- >>> pretty (bool "." "x" <$> topBs (m Map.! "o2") 0.25 :: RangedHand Text.Text)
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
topBs :: RangedHand Double -> Double -> RangedHand Bool
topBs bs x = tabulate (`elem` top)
  where
    sortedList x = second snd <$> sortOn (Down . fst . snd) (toList (liftR2 (,) (tabulate id) (liftR2 (,) x (fromIntegral <$> handTypeCount))))
    (total, as) = second reverse $ foldl' (\(c', xs) (b, c) -> (c' + c, (b, c' + c) : xs)) (0, []) (sortedList bs)
    cut = x * total
    top = fst <$> List.takeWhile ((< cut) . snd) as

-- | Construct a RangedHand RawAction that chooses to (Raise r) for the top x% of hands, or Call for the top y%, and thus Fold for the bottom (1-y)%.
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
rcf :: RangedHand Double -> Double -> Double -> Double -> RangedHand RawAction
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
ev :: Int -> Int -> [RangedHand RawAction] -> Maybe Double
ev n sims acts =
  listToMaybe $
    fmap ((+ negate 10) . (/ fromIntegral sims) . sum) $
      List.transpose $
        evs n sims acts

-- | Simulate winnings for each seat.
--
-- >>> all (==20.0) $ sum <$> evs 2 100 [rcf s 1 0 0.9, rcf s 1 0.3 0.9, rcf s 1 0.1 0.5, rcf s 1 0.6 0.8]
-- True
evs :: Int -> Int -> [RangedHand RawAction] -> [[Double]]
evs n sims acts = fmap (toList . stacks) (evTables n sims acts)

-- | Simulate end state of tables given strategies.
evTables :: Int -> Int -> [RangedHand RawAction] -> [Table]
evTables n sims acts =
  showdown . applies acts <$> tables
  where
    cards = evalState (replicateM sims (dealN (5 + 2 * n))) (mkStdGen 42)
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
ev2Ranges :: RangedHand Double -> Int -> [Double] -> Maybe Double
ev2Ranges s sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev 2 sims [rcf s 10 s0r s0c, rcf s 10 s1r 1, rcf s 10 0 s2r, rcf s 10 0 s3r]
ev2Ranges _ _ _ = Nothing

-- | Apply a 'RangedHand' 'RawAction', supplying a RawAction for the cursor.
--
-- >>> apply (always RawCall) t
-- RawCall
apply :: RangedHand RawAction -> Table -> RawAction
apply s t = fromMaybe RawFold $ case cursor t of
  Nothing -> error "bad juju"
  Just i -> Just $ index s (from shapedHandS $ fromHand (playerCards (cards t) List.!! i))

-- | Apply a list of 'RangedHand' 'RawAction's, until betting is closed, or the list ends.
--
-- >>> pretty $ applies (always (RawRaise 10) : (replicate 3 (always RawCall))) t
-- Ac7s Tc5s|6d7c6s|9c|4s,hero: ,c c,0 0,10 10,0,c1:9.0r0
applies :: [RangedHand RawAction] -> Table -> Table
applies ss t = go ss t
  where
    go [] t = t
    go (s : ss') t =
      bool (applies ss' (actOn (apply s t) t)) t (closed t)

-- | deal n cards given a Hand has been dealt.
--
-- >>> pretty $ evalState (dealShapedHand (MkPair Ace) 7) (mkStdGen 42)
-- Ah7sTc5s6d7c6s
dealShapedHand :: (RandomGen g) => ShapedHand -> Int -> State g CardsS
dealShapedHand b n =
  dealNWith n
    . ( \(x, y) ->
          let (x', y') =
                bool (y, x) (x, y) (x <= y)
           in S.splitAt x' (unwrapCards allCardsS)
                & second (S.splitAt (y' - x'))
                & (\(t, (t', t'')) -> t <> S.tail t' <> S.tail t'')
                & CardsS
      )
    . (\(Hand x y) -> (fromIntegral (unwrapCard (from cardS x)), fromIntegral (unwrapCard (from cardS y))))
    . toRepHand
    $ b

-- | deal a table given player i has been dealt a B
--
-- >>> pretty $ evalState (dealTableHand defaultTableConfig 0 (MkPair Ace)) (mkStdGen 42)
-- AcAd Ah7s|Tc5s6d|7c|6s,hero: 0,o o,9.5 9,0.5 1,0,
dealTableHand :: (RandomGen g) => TableConfig -> Int -> ShapedHand -> State g Table
dealTableHand cfg i b = do
  (CardsS xs) <- dealShapedHand b (5 + (tableSize cfg - 1) * 2)
  pure $
    makeTableS cfg $
      CardsS $
        S.take (2 * i) xs
          <> (\(Hand x y) -> unwrapCards $ from cardsS [x, y]) (toRepHand b)
          <> S.drop (2 * i) xs

-- | create a list of n dealt tables, with p players, where b is dealt to player k
--
-- >>> pretty $ tablesB 2 (MkPair Ace) 1 3
-- [ Ah7s AcAd|Tc5s6d|7c|6s,hero: 0,o o,9.5 9,0.5 1,0,
-- , 7s4s AcAd|Td3d6c|Kh|Ts,hero: 0,o o,9.5 9,0.5 1,0,
-- , 9c8s AcAd|Ks2h4h|5d|Tc,hero: 0,o o,9.5 9,0.5 1,0, ]
tablesB :: Int -> ShapedHand -> Int -> Int -> [Table]
tablesB p b k n =
  evalState
    ( replicateM
        n
        (dealTableHand (defaultTableConfig {tableSize = p}) k b)
    )
    (mkStdGen 42)

-- | create some common simulation results
someRanges :: Int -> Map.Map Text (RangedHand Double)
someRanges n = do
  Map.fromList
    [ ("o2", tabulate (\b -> winHand (to shapedHandS b) 2 n)),
      ("o9", tabulate (\b -> winHand (to shapedHandS b) 9 n)),
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

-- | read RangedHand map
-- FIXME:
--
-- >>> (Just m) <- readSomeRanges
-- >>> index (m Map.! "o2") (from shapedHandS $ MkSuited Jack Ten)
-- 0.5742
readSomeRanges :: IO (Maybe (Map.Map Text (RangedHand Double)))
readSomeRanges = do
  t <- readFile "other/some.str"
  pure $ readMaybe t
