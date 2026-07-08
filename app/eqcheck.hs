{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad.State.Lazy (evalState)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Random (mkStdGen)
import Poker.Card (Rank (..))
import Poker.EquityMatrix
import Poker.EquitySuit
import Poker.HandRank (hvs7)
import Optics.Core (view)
import Poker.Range (StartingHand (..), StartingHandS (..), fromHole, readSomeRanges, ev2Ranges, startingHandI, toRepHole)
import Poker.Strategy
import Prelude

-- | A strategy that always folds where legal (fold threshold at 1.0).
alwaysFold :: Strategy
alwaysFold = Strategy (replicate 5 (1.0, 1.0, 1.0, 1.0))

-- | A strategy that always calls (fold threshold 0, call threshold 1.0).
alwaysCall :: Strategy
alwaysCall = Strategy (replicate 5 (0.0, 1.0, 1.0, 1.0))

main :: IO ()
main = do
  hvs <- hvs7
  ranges <- fromMaybe (error "no ranges") <$> readSomeRanges
  let o2 = ranges Map.! "o2"

  exists <- doesFileExist "other/eqmatrix.str"
  eq <- if exists then readEquityMatrix
    else evaluate (buildEquityMatrix hvs 100_000)

  suitExists <- doesFileExist "other/eqmatrixSuit.str"
  eqSuit <-
    if suitExists
      then do
        putStrLn "loading other/eqmatrixSuit.str"
        readSuitEquityMatrix
      else do
        putStrLn "other/eqmatrixSuit.str not found, building with 5M deals..."
        evaluate (buildEquityMatrixSuit hvs 5_000_000)

  putStrLn "Direct walkPair tests:"
  putStrLn $ "  alwaysFold AA vs AA: " ++ show (walkPair eq o2 alwaysFold alwaysCall 168 168)
  putStrLn $ "  alwaysFold 72o vs AA: " ++ show (walkPair eq o2 alwaysFold alwaysCall 5 168)
  putStrLn $ "  alwaysFold AA vs 72o: " ++ show (walkPair eq o2 alwaysFold alwaysCall 168 5)

  putStrLn $ "  alwaysCall AA vs AA: " ++ show (walkPair eq o2 alwaysCall alwaysCall 168 168) ++ "  (expect ~0: pot=2, equity~0.5, stacks reset → 0)"
  putStrLn $ "  alwaysCall AA vs 72o: " ++ show (walkPair eq o2 alwaysCall alwaysCall 168 5)

  putStrLn "\nBaseline walkPair:"
  putStrLn $ "  AA vs AA: " ++ show (walkPair eq o2 seat0Baseline seat0Baseline 168 168)
  putStrLn $ "  AA vs 72o: " ++ show (walkPair eq o2 seat0Baseline seat0Baseline 168 5)
  putStrLn $ "  72o vs AA: " ++ show (walkPair eq o2 seat0Baseline seat0Baseline 5 168)
  putStrLn $ "  T9s vs JTo: " ++ show (walkPair eq o2 seat0Baseline seat0Baseline 87 96)

  putStrLn "\nMC ev2Ranges:"
  putStrLn $ "  baseline 200k: " ++ show (ev2Ranges hvs o2 200000 (strategyToTheta seat0Baseline))
  putStrLn $ "  alwaysFold: " ++ show (ev2Ranges hvs o2 50000 [0, 0, 0, 0, 0])

  putStrLn "\nevStrategy (playOne MC, 200k hands):"
  putStrLn $ "  baseline: " ++ show (evalState (evStrategy hvs o2 seat0Baseline seat0Baseline 200000) (mkStdGen 42))
  putStrLn $ "  alwaysCall: " ++ show (evalState (evStrategy hvs o2 alwaysCall alwaysCall 200000) (mkStdGen 42))

  putStrLn "\nAnalytical full:"
  putStrLn $ "  baseline: " ++ show (evAnalytical eq o2 seat0Baseline seat0Baseline)
  putStrLn $ "  alwaysCall: " ++ show (evAnalytical eq o2 alwaysCall alwaysCall)

  putStrLn "\nSuit-pattern diagnostic (32o vs AKo):"
  let idx32o = fromEnum . unStartingHandS $ view startingHandI (fromHole (toRepHole (Offsuited Three Two)))
      idxAKo = fromEnum . unStartingHandS $ view startingHandI (fromHole (toRepHole (Offsuited Ace King)))
  mapM_ (\(p, pr) -> putStrLn $ "  " ++ show p ++ ": " ++ show pr) (Map.toList (suitPatternProbs idx32o idxAKo))

  putStrLn "\nSuit-aware analytical:"
  putStrLn $ "  baseline: " ++ show (evAnalyticalSuit eqSuit o2 seat0Baseline seat0Baseline)
  putStrLn $ "  alwaysCall: " ++ show (evAnalyticalSuit eqSuit o2 alwaysCall alwaysCall)
