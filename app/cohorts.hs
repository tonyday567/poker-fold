{-# LANGUAGE OverloadedStrings #-}

-- | Run a small matrix of cohort strategies against each other and print
-- the seat-0 EV in bb/hand.  The goal is a handful of rules of thumb
-- about width, aggression, and the cost of tightening up.
module Main where

import Control.Monad.State.Lazy (evalState)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.FormatN (fixed)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Poker.HandRank (hvs7)
import Poker.Range (readSomeRanges)
import Poker.Strategy
  ( Strategy (..),
    compactProfile,
    evStrategy,
    seat0Baseline,
    seat1Baseline,
  )
import System.Random (mkStdGen)
import Prelude

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

sort4 :: (Double, Double, Double, Double) -> (Double, Double, Double, Double)
sort4 (a, b, c, d) = case List.sort [a, b, c, d] of
  [a', b', c', d'] -> (a', b', c', d')
  _ -> error "sort4: expected four thresholds"

-- | Shift every threshold up (tighter) or down (wider).
shiftAll :: Double -> Strategy -> Strategy
shiftAll f (Strategy ts) =
  Strategy $
    map
      (\(a, b, c, d) -> sort4 (clamp01 (a + f), clamp01 (b + f), clamp01 (c + f), clamp01 (d + f)))
      ts

-- | Shift only the raise/all-in thresholds up (more passive) or down
-- (more aggressive), keeping the fold/call boundaries fixed.
shiftRaises :: Double -> Strategy -> Strategy
shiftRaises f (Strategy ts) =
  Strategy $
    map
      (\(a, b, c, d) -> sort4 (a, b, clamp01 (c + f), clamp01 (d + f)))
      ts

data Cohort = Cohort
  { sb :: Strategy,
    bb :: Strategy
  }

cohorts :: [(String, Cohort)]
cohorts =
  [ ("baseline", Cohort seat0Baseline seat1Baseline),
    ("slightlyWide", Cohort (shiftAll (-0.05) seat0Baseline) (shiftAll (-0.05) seat1Baseline)),
    ("wide", Cohort (shiftAll (-0.15) seat0Baseline) (shiftAll (-0.15) seat1Baseline)),
    ("slightlyTight", Cohort (shiftAll 0.05 seat0Baseline) (shiftAll 0.05 seat1Baseline)),
    ("tight", Cohort (shiftAll 0.15 seat0Baseline) (shiftAll 0.15 seat1Baseline)),
    ("alwaysCall", Cohort (Strategy (replicate 5 (0, 1, 1, 1))) (Strategy (replicate 5 (0, 1, 1, 1))))
  ]

main :: IO ()
main = do
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"
  let n = 100000
  let gen0 = mkStdGen 2025

  putStrLn "Cohort profiles (SB / BB)"
  putStrLn ""
  mapM_ (\(name, c) -> do
    putStrLn (name ++ " SB: " ++ compactProfile (sb c) (sb c))
    putStrLn (name ++ " BB: " ++ compactProfile (bb c) (bb c))
    putStrLn "") cohorts

  putStrLn $ "Seat-0 EV in bb/100 hands over " ++ show n ++ " hands"
  putStrLn $ "Rows are the hero cohort (playing SB); columns are the opponent cohort (playing BB)."
  putStrLn ""

  -- header
  putStr $ pad 14 ""
  mapM_ (\(name, _) -> putStr (pad 14 name)) cohorts
  putStrLn ""

  let ev (hero, _) (opp, _) = evalState (evStrategy hvs o2 (sb hero) (bb opp) n) gen0
  mapM_
    ( \(hName, hCoh) -> do
        putStr (pad 14 hName)
        mapM_
          ( \(oName, oCoh) -> do
              let e = ev (hCoh, hName) (oCoh, oName)
              putStr (pad 14 (showEV e))
          )
          cohorts
        putStrLn ""
    )
    cohorts

pad :: Int -> String -> String
pad n s = take n (s ++ replicate n ' ')

-- | EV in bb/100 hands, rounded to two decimals.
showEV :: Double -> String
showEV x =
  let s = if x >= 0 then "+" else ""
   in s ++ unpack (fixed (Just 2) (x * 100))
