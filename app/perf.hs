{-# LANGUAGE OverloadedStrings #-}

module Main where

import Circuit.Meter.Time (ticksN, ticksION)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Control.Monad.State.Lazy (evalState)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Data.Word (Word8)
import Poker.Card (Card (..), Hole (..))
import Poker.Card.Storable (CardS (..), CardsS (..), unwrapCardsS)
import Poker.HandRank (lookupHR, lookupHRUnsorted, sort, hvs7)
import Poker.Lexico (toLexiPosR)
import Poker.Random (dealN, dealTable, indexShuffle, rvi, rviv)
import Poker.Range (readSomeRanges)
import Poker.Strategy (Strategy (..), evStrategy, playOne, seat0Baseline, seat1Baseline)
import Poker.Table (Table, TableCards (..), cards, defaultTableConfig, showdown, stacks)
import System.Random (mkStdGen)
import Prelude

bench :: (NFData b) => String -> Int -> Int -> (Int -> b) -> IO ()
bench label runs n f = do
  (totalNs, _) <- ticksN runs f n
  let perNs = totalNs `div` fromIntegral n
  putStrLn $ label ++ ": " ++ show perNs ++ " ns (" ++ show (fromIntegral perNs / 1000 :: Double) ++ " us)"

benchIO :: String -> Int -> IO a -> IO ()
benchIO label n action = do
  (avgNs, _) <- ticksION n action
  putStrLn $ label ++ ": " ++ show avgNs ++ " ns (" ++ show (fromIntegral avgNs / 1000 :: Double) ++ " us)"

sumCardsV :: S.Vector Word8 -> Int
sumCardsV v = S.foldl' (+) 0 (S.map fromIntegral v)

cardSum :: Table -> Int
cardSum t =
  let TableCards holes (f0, f1, f2) tc rc = cards t
      cardVal (Card r s) = fromEnum r + fromEnum s
      holeSum = sum [cardVal c1 + cardVal c2 | Hole c1 c2 <- holes]
      boardSum = fromIntegral (unwrapCardS f0 + unwrapCardS f1 + unwrapCardS f2 + unwrapCardS tc + unwrapCardS rc)
   in holeSum + boardSum

main :: IO ()
main = do
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"
  let gen0 = mkStdGen 7
  let runs = 100
  let n = 1000

  -- 1. raw random variates
  let rvN k = evalState (replicateM k (rvi 52)) gen0 :: [Int]
  bench "rvi 52" runs n (sum . rvN)

  -- 2. index shuffle for a 9-card sample
  let shuffle9 k = evalState (replicateM k (indexShuffle . S.toList <$> rviv 52 9)) gen0 :: [[Int]]
  bench "indexShuffle 9" runs n (sum . map sum . shuffle9)

  -- 3. full deal of 9 cards
  let deal9 k = evalState (replicateM k (dealN 9)) gen0 :: [CardsS]
  bench "dealN 9" runs n (sum . map (sumCardsV . unwrapCardsS) . deal9)

  -- 4. deal a whole table
  let dealT k = evalState (replicateM k (dealTable defaultTableConfig)) gen0 :: [Table]
  bench "dealTable" runs n (sum . map cardSum . dealT)

  -- precompute sorted 7-card sets for pure benchmarks
  let cssRaw = evalState (replicateM n (dealN 7)) gen0
  let cssSorted = map (CardsS . sort . unwrapCardsS) cssRaw

  -- 5. lexicographic index computation only
  bench "toLexiPosR 52 7" runs n (\_ ->
    (foldl' (\acc cs -> acc + toLexiPosR (52 :: Int) (7 :: Int) (unwrapCardsS cs :: S.Vector Word8)) 0 cssSorted) :: Int)

  -- 6. lookup in hvs7 (sorted input) -- note: ticksN can memoise this fold
  bench "lookupHR sorted" runs n (\_ ->
    (foldl' (\acc cs -> acc + fromIntegral (lookupHR hvs cs)) 0 cssSorted) :: Int)

  -- 7. IO-backed variants.  These use an IORef index so GHC cannot float
  -- or memoise the work, giving a true per-iteration cost.
  let nIO = n + 100
  cssRawV <- pure $ force $ V.fromList $ evalState (replicateM nIO (dealN 7)) (mkStdGen 8)
  let cssSortedV = force $ V.map (CardsS . sort . unwrapCardsS) cssRawV
  let cssUnsortedV = cssRawV

  idxSort <- newIORef 0
  sumSort <- newIORef (0 :: Int)
  benchIO "sort 7-card vector (IO)" n $ do
    i <- readIORef idxSort
    let !cs = cssUnsortedV V.! i
    let !v = sort (unwrapCardsS cs)
    let !r = sumCardsV v
    writeIORef idxSort (i + 1)
    modifyIORef' sumSort (+ r)
    evaluate (force r)

  idxLookSorted <- newIORef 0
  sumLookSorted <- newIORef (0 :: Int)
  benchIO "lookupHR sorted (IO)" n $ do
    i <- readIORef idxLookSorted
    let !cs = cssSortedV V.! i
    let !r = fromIntegral (lookupHR hvs cs) :: Int
    writeIORef idxLookSorted (i + 1)
    modifyIORef' sumLookSorted (+ r)
    evaluate (force r)

  idxLookUnsorted <- newIORef 0
  sumLookUnsorted <- newIORef (0 :: Int)
  benchIO "lookupHR unsorted (IO)" n $ do
    i <- readIORef idxLookUnsorted
    let !cs = cssUnsortedV V.! i
    let !r = fromIntegral (lookupHRUnsorted hvs cs) :: Int
    writeIORef idxLookUnsorted (i + 1)
    modifyIORef' sumLookUnsorted (+ r)
    evaluate (force r)

  idxLookSortedHot <- newIORef 0
  sumLookSortedHot <- newIORef (0 :: Int)
  benchIO "lookupHR sorted hot (IO)" n $ do
    i <- readIORef idxLookSortedHot
    let !cs = cssSortedV V.! i
    let !r = fromIntegral (lookupHR hvs cs) :: Int
    writeIORef idxLookSortedHot (i + 1)
    modifyIORef' sumLookSortedHot (+ r)
    evaluate (force r)

  -- precompute tables for showdown / play benchmarks
  tables <- pure $ evalState (replicateM n (dealTable defaultTableConfig)) gen0

  -- 8. showdown
  bench "showdown" runs n (\_ ->
    (foldl' (\acc t -> let r = sum (stacks (showdown hvs t)) in r `seq` acc + (round r :: Int)) 0 tables) :: Int)

  -- 9. full play
  bench "playOne baseline" runs n (\k ->
    (round (sum (map (playOne hvs o2 seat0Baseline seat1Baseline) (take k tables))) :: Int))

  -- 10. full ev loop
  bench "evStrategy baseline" runs n (\k ->
    evalState (evStrategy hvs o2 seat0Baseline seat1Baseline k) gen0)

  let alwaysCall = Strategy [(0, 1, 1, 1), (0, 1, 1, 1), (0, 1, 1, 1), (0, 1, 1, 1), (0, 1, 1, 1)]
  let bigEv = evalState (evStrategy hvs o2 alwaysCall alwaysCall 100000) gen0
  putStrLn $ "alwaysCall vs alwaysCall EV (100k): " ++ show bigEv
