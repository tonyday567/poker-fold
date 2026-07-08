{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main where

import Control.Monad (forM_, void, when)
import Data.Function (on)
import Data.Functor.Rep (index)
import Data.List (maximumBy)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.FormatN (fixed)
import Data.Text (unpack)
import Poker.CFR
import Poker.EquityMatrix (readEquityMatrix)
import Poker.Range (Range, StartingHandS (..), readSomeRanges)
import Prelude

main :: IO ()
main = do
  putStrLn "=== poker-fold CFR Solver ==="

  eq <- readEquityMatrix
  ranges <- readSomeRanges
  let o2 = case ranges of
        Just m -> m Map.! "o2"
        Nothing -> error "Could not load o2 data"

  putStrLn "Loaded equity matrix and o2 data"

  let iterations = 100
  (strat, evs) <- runCFR eq iterations

  putStrLn "\n=== Convergence ==="
  putStrLn "Iter | Self-play EV"
  forM_ (zip [1 :: Int ..] evs) $ \(i, ev) ->
    when (i <= 10 || i `mod` 10 == 0) $
      putStrLn $ show i ++ " | " ++ show ev

  putStrLn "\n=== Final CFR Strategy ==="
  printStrategySummary strat o2
  printFrequencies strat o2
  void (exploitability eq strat)

printStrategySummary :: StrategyTable -> Range Double -> IO ()
printStrategySummary st o2 = do
  putStrLn "Context | Hand sample | Dominant action (prob)"
  putStrLn "--------|-------------|------------------------"
  forM_ [0 .. 5 :: Int] $ \ctx -> do
    let samples = [0, 30, 60, 90, 120, 150, 168]
    forM_ samples $ \hc -> do
      let acts = contextActions ctx
          probs = [stratProb st ctx hc a | a <- acts]
          best = maximumBy (compare `on` snd) (zip acts probs)
          o2val = index o2 (StartingHandS (fromIntegral hc))
          fmt = unpack . fixed (Just 3)
      putStrLn $ "  C" ++ show ctx ++ " | hc" ++ show hc ++ "(o2=" ++ fmt o2val ++ ") | "
        ++ show (fst best) ++ "(" ++ fmt (snd best) ++ ")"

printFrequencies :: StrategyTable -> Range Double -> IO ()
printFrequencies st _o2 = do
  putStrLn "\n=== Action frequencies (combo-weighted) ==="
  forM_ [0 .. 5 :: Int] $ \ctx -> do
    let acts = contextActions ctx
        hcs = [0 .. 168]
        actionCount a = sum [stratProb st ctx hc a * handComboWeight hc | hc <- hcs]
        total = sum [handComboWeight hc | hc <- hcs]
    putStrLn $ "C" ++ show ctx ++ ": " ++
      List.intercalate "  " [show a ++ "=" ++ unpack (fixed (Just 1) (actionCount a / total * 100)) ++ "%"
                            | a <- acts]
