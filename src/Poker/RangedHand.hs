{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | RangedHand computations
--
--
module Poker.RangedHand
  ( -- * RangedHand
    topBs,
    ev,
    evs,
    ev2Ranges,
    winHand,
    winOdds,
    rcf,

    fromRawAction,
    fromRawActionType,
    someRanges,
    writeSomeRanges,
    readSomeRanges,

  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Lens.Micro
import Prelude
import Poker.Random
import Poker.Types
import Poker.Evaluate
import Control.Monad.State.Lazy
import GHC.OverloadedLabels
import Data.Bifunctor
import Data.Text (Text, unpack, pack)
import Data.List (sort, sortOn)
import Data.Ord
import System.Random
import Text.Read (readMaybe)
import Data.Bool
import Data.Foldable
import Data.Maybe
import Poker
import Data.Functor.Rep

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Poker
-- >>> import Poker.Types
-- >>> import Poker.RangedHand
-- >>> import Poker.Random
-- >>> import qualified Data.Map.Strict as Map
-- >>> import Lens.Micro
-- >>> import Prelude
-- >>> import Control.Monad.State.Lazy
-- >>> import System.Random
-- >>> import Prettyprinter
-- >>> import Lens.Micro
-- >>> import Prelude
-- >>> import qualified Data.Text as Text
-- >>> import Data.Functor.Rep
-- >>> import Data.Bool
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
--

-- | create some common enumeration/simulation results in RangedHandshape, from scratch.
someRanges :: Int -> Map.Map Text (RangedHand Double)
someRanges n = do
  Map.fromList
    [ ("o2", tabulate (\b -> winHand (riso shapedHandS b) 2 n)),
      ("o9", tabulate (\b -> winHand (riso shapedHandS b) 9 n)),
      ("count", handTypeCount),
      ("freq", (/ sum handTypeCount) <$> handTypeCount)
    ]

-- | write RangedHand results to file.
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
-- >>> index (m Map.! "o2") (liso shapedHandS $ MkSuited Jack Ten)
-- 0.5742
readSomeRanges :: IO (Maybe (Map.Map Text (RangedHand Double)))
readSomeRanges = do
  t <- readFile "other/some.str"
  pure $ readMaybe t

-- | Given a B, what is the chance of that player winning against p other players, simulated n times.
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
winOdds p n = tabulate (\b -> winHand (riso shapedHandS b) p n)

-- | Top x percent of hands, order determined by a RangedHand Double, for n-seated.
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
    sortedBList x = second snd <$> sortOn (Down . fst . snd) (toList (liftR2 (,) (tabulate id) (liftR2 (,) x handTypeCount)))
    (total, as) = second reverse $ foldl' (\(c', xs) (b, c) -> (c' + c, (b, c' + c) : xs)) (0, []) (sortedBList bs)
    cut = x * total
    top = fst <$> List.takeWhile ((< cut) . snd) as

-- | convert an RawAction top "f","c", or "r"
fromRawAction :: RawAction -> Text
fromRawAction = fromRawActionType ("f", "c", "r")

-- | Convert from an RawAction to a triple representing fold, call or raise.
fromRawActionType :: (a, a, a) -> RawAction -> a
fromRawActionType (a, _, _) RawFold = a
fromRawActionType (_, a, _) RawCall = a
fromRawActionType (_, _, a) (RawRaise _) = a

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
    (\b -> bool (bool RawFold RawCall (index (topBs s y) b))
      (RawRaise r) (index (topBs s x) b))

-- | Simulate the expected value of a strategy
--
-- FIXME: nefarious duplicate card bug
--
-- >>> :set -XOverloadedLabels
-- >>> cards = evalState (replicateM 10 (dealN (5 + 2 * 2))) (mkStdGen 42)
-- >>> acts = [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- >>> ts = makeTable (defaultTableConfig & #numPlayers .~ 2) <$> cards
--
-- >>> pretty ts
-- [ Ac7s Tc5s|6d7c6s|9c|4s,hero: 0,o o,9.5 9,0.5 1,0,
-- , 9sAs 3d5s|KcTd9h|9d|2h,hero: 0,o o,9.5 9,0.5 1,0,
-- , 4d5c 9d8h|6sJd4s|Qc|Qc,hero: 0,o o,9.5 9,0.5 1,0,
-- , Qs9s Qd8s|Jc6h4c|Ah|8c,hero: 0,o o,9.5 9,0.5 1,0,
-- , 9sJs 5h2s|Jc8c6h|Tc|5s,hero: 0,o o,9.5 9,0.5 1,0,
-- , 2d5c Qc3c|4d3hKs|Th|Qc,hero: 0,o o,9.5 9,0.5 1,0,
-- , 5dKd 9s9d|9c6s2c|3c|4h,hero: 0,o o,9.5 9,0.5 1,0,
-- , Kc9c TsQs|3d6sKd|7d|9d,hero: 0,o o,9.5 9,0.5 1,0,
-- , KdAc 4c6s|6c8d6c|5s|3c,hero: 0,o o,9.5 9,0.5 1,0,
-- , KdQh Ah9h|Ad4s2d|Kh|8d,hero: 0,o o,9.5 9,0.5 1,0, ]
--
-- >>> ev 2 100 [rcf s 10 0.2 0.9, rcf s 10 0.3 0.9, rcf s 10 0.1 0.5, rcf s 10 0.6 0.8]
-- Just 0.5500000000000007
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
evs n sims acts = fmap (\x -> toList $ x ^. #stacks) (evTables n sims acts)

-- | Simulate end state of tables given strategies.
evTables :: Int -> Int -> [RangedHand RawAction] -> [Table]
evTables n sims acts =
  showdown . bet acts <$> tables
  where
    cards = evalState (replicateM sims (dealN (5 + 2 * n))) (mkStdGen 42)
    tables = makeTable (defaultTableConfig & #numPlayers .~ n) <$> cards

-- | Simulate the expected value of a 2 seat game, given the 5 decision point cuts of headsup described in 'actOn'.
--
-- aka bug detector.
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
-- Just 0.3949999999999996
--
-- [0,1,0,_,_] is iso to always Call
--
-- >>> ev2Ranges s 100 [0,1,0,1,1]
-- Just 3.500000000000014e-2
ev2Ranges :: RangedHand Double -> Int -> [Double] -> Maybe Double
ev2Ranges s sims (s0r : s0c : s1r : s2r : s3r : _) =
  ev 2 sims [rcf s 10 s0r s0c, rcf s 10 s1r 1, rcf s 10 0 s2r, rcf s 10 0 s3r]
ev2Ranges _ _ _ = Nothing
