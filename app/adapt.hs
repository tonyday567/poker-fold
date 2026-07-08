{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.State.Lazy (State, evalState)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Poker.Card (Hole)
import Poker.HandRank (hvs7)
import Poker.Random (dealTable)
import Poker.Range (Range, readSomeRanges)
import Poker.SimpleBelief
import Poker.Strategy
import Poker.Table
import System.Environment (getArgs)
import System.Random (RandomGen, mkStdGen)
import Text.Read (readMaybe)
import Prelude

argValue :: String -> [String] -> Maybe String
argValue _ [] = Nothing
argValue key (k : v : rest) | k == key = Just v
argValue key (_ : rest) = argValue key rest

-- ============================================================================
-- Level 1: Hero adapts, opponent fixed (original adapt.hs)
-- ============================================================================

heroSeat, oppSeat :: Int
heroSeat = 0
oppSeat = 1

data HandResult = HandResult
  { finalTable :: Table,
    oppObs :: [Obs],
    heroProfit :: Double
  }
  deriving (Eq, Show)

playHand :: S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Table -> HandResult
playHand hvs o2 hero opp t0 = go t0 []
  where
    go t obs = case cursor t of
      Nothing ->
        let t' = showdown hvs t
            profit = stacks t' !! heroSeat - 10.0
         in HandResult t' obs profit
      Just p ->
        let ctx = fromMaybe (error "playHand: no decision context") (decisionContext t)
            hole = playerCards (cards t) !! p
            raw = strategyAction o2 (if p == heroSeat then hero else opp) ctx hole
            t' = actOn raw t
            obs' = if p == oppSeat then Obs ctx hole raw : obs else obs
         in go t' obs'

runHands ::
  (RandomGen g) =>
  S.Vector Word16 ->
  Range Double ->
  Strategy ->
  Belief ->
  Int ->
  State g [(Int, Double, Double)]
runHands hvs o2 opp b0 n = go 1 b0 []
  where
    go i belief acc
      | i > n = pure (reverse acc)
      | otherwise = do
          let hero = adaptHero belief
          t0 <- dealTable defaultTableConfig
          let result = playHand hvs o2 hero opp t0
              belief' = foldl (\b (Obs ctx _ raw) -> updateBelief b ctx raw) belief (oppObs result)
              c2 = case thresholds (adaptHero belief') !! 2 of (t, _, _, _) -> t
          go (i + 1) belief' ((i, heroProfit result, c2) : acc)

printSummary :: [(Int, Double, Double)] -> IO ()
printSummary rows = do
  let total = sum [p | (_, p, _) <- rows]
  putStrLn $ "total hands: " ++ show (length rows)
  putStrLn $ "cumulative hero profit: " ++ show total
  putStrLn "hand | profit | cum | c2"
  go rows 0
  where
    go [] _ = pure ()
    go ((i, p, c2) : rest) cum = do
      let cum' = cum + p
      when (i <= 5 || i `mod` 10 == 0) $
        putStrLn $ show i ++ " | " ++ show p ++ " | " ++ show cum' ++ " | " ++ show c2
      go rest cum'

-- ============================================================================
-- Level 2: Both players adapt via count-based beliefs about each other
-- ============================================================================

-- | Result of one hand with mutual adaptation: both players' observations
-- and the profit for seat 0.
data MutualResult = MutualResult
  { mrObs0 :: [Obs],    -- what seat 0 observed about seat 1
    mrObs1 :: [Obs],    -- what seat 1 observed about seat 0
    mrProfit :: Double  -- seat 0 profit
  }

-- | Play one hand where both players use pre-derived strategies.
-- Observations are collected for belief updates.
playHandMutual :: S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Table -> MutualResult
playHandMutual hvs o2 s0 s1 t0 = go t0 [] []
  where
    go t obs0 obs1 = case cursor t of
      Nothing ->
        let t' = showdown hvs t
            profit = stacks t' !! 0 - 10.0
         in MutualResult obs0 obs1 profit
      Just p ->
        let ctx = fromMaybe (error "playHandMutual: no decision context") (decisionContext t)
            hole = playerCards (cards t) !! p
            raw = strategyAction o2 (if p == 0 then s0 else s1) ctx hole
            t' = actOn raw t
            obs0' = if p == 1 then Obs ctx hole raw : obs0 else obs0
            obs1' = if p == 0 then Obs ctx hole raw : obs1 else obs1
         in go t' obs0' obs1'

-- | Format a strategy as a single line of context abbreviations.
compactStrategyLine :: Strategy -> String
compactStrategyLine (Strategy ts) =
  List.intercalate " " [fmt ctx tup | (ctx, tup) <- zip [0..5] ts]
  where
    fmt ctx (t0, t1, t2, t3) =
      let acts = nextActionCounts ctx (t0, t1, t2, t3)
       in "C" ++ show ctx ++ ":" ++ unwords [show p ++ "%" | p <- acts]
    nextActionCounts ctx (t0, t1, t2, t3) = case ctx of
      0 -> [round (t0*100), round ((t1-t0)*100), round ((t2-t1)*100), round ((t3-t2)*100)]
      1 -> [round (t1*100), round ((t2-t1)*100), round ((t3-t2)*100)]
      2 -> [round (t0*100), round ((1-t0)*100)]
      3 -> [round (t0*100), round ((1-t0)*100)]
      4 -> [round (t0*100), round ((t1-t0)*100), round ((1-t1)*100)]
      5 -> [round (t0*100), round ((1-t0)*100)]
      _ -> []

-- ============================================================================
-- Main
-- ============================================================================

-- | Player type: how strategy is derived from belief.
data PlayerType = L0 | L1 | L2
  deriving (Eq, Show, Read)

-- | Derive strategy from belief based on player type and seat.
playerStrategy :: PlayerType -> Int -> Belief -> Strategy
playerStrategy L0 p _ = if p == 0 then seat0Baseline else seat1Baseline
playerStrategy L1 _ b = adaptHero b    -- adaptHero is SB-centric; BB-L1 is approximate
playerStrategy L2 _ b = beliefToStrategy b

-- | Play N hands where BOTH players use belief-derived strategies.
-- SB uses ps0, BB uses ps1. Both observe and update their beliefs.
runMatchup ::
  (RandomGen g) =>
  S.Vector Word16 ->
  Range Double ->
  PlayerType -> PlayerType ->
  Int -> Double ->
  State g [(Int, Double, Strategy, Strategy)]
runMatchup hvs o2 pt0 pt1 n priorStr = go 1 b0init b1init []
  where
    b0init = initialBeliefWithStrength seat0Baseline priorStr
    b1init = initialBeliefWithStrength seat1Baseline priorStr
    go i b0 b1 acc
      | i > n = pure (reverse acc)
      | otherwise = do
          let s0 = playerStrategy pt0 0 b0
              s1 = playerStrategy pt1 1 b1
          t0 <- dealTable defaultTableConfig
          let MutualResult obs0 obs1 profit = playHandMutual hvs o2 s0 s1 t0
              -- Always update beliefs (L0 just ignores belief when generating strategy)
              b0' = foldl (\b (Obs ctx _ raw) -> updateBelief b ctx raw) b0 obs0
              b1' = foldl (\b (Obs ctx _ raw) -> updateBelief b ctx raw) b1 obs1
              s0' = playerStrategy pt0 0 b0'
              s1' = playerStrategy pt1 1 b1'
          go (i + 1) b0' b1' ((i, profit, s0', s1') : acc)

-- | Play N hands where one player has a FIXED strategy (not belief-based).
-- SB adapts, BB is fixed.
runVsFixed ::
  (RandomGen g) =>
  S.Vector Word16 ->
  Range Double ->
  PlayerType ->  -- SB's adaptation type
  Strategy ->    -- BB's fixed strategy
  Int -> Double ->
  State g [(Int, Double, Strategy)]
runVsFixed hvs o2 pt opp n priorStr = go 1 b0init []
  where
    b0init = initialBeliefWithStrength seat0Baseline priorStr
    go i b0 acc
      | i > n = pure (reverse acc)
      | otherwise = do
          let s0 = playerStrategy pt 0 b0
          t0 <- dealTable defaultTableConfig
          let result = playHand hvs o2 s0 opp t0
              b0' = foldl (\b (Obs ctx _ raw) -> updateBelief b ctx raw) b0 (oppObs result)
              s0' = playerStrategy pt 0 b0'
          go (i + 1) b0' ((i, heroProfit result, s0') : acc)

main :: IO ()
main = do
  args <- getArgs
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"
  let nHands = 200
      priorStr = case argValue "--prior" args >>= (readMaybe :: String -> Maybe Double) of
                   Just n -> n
                   Nothing -> 5

  let mode
        | "--l0-vs-l0" `elem` args = ("--l0-vs-l0", (L0, L0, False))
        | "--l1-vs-l0" `elem` args = ("--l1-vs-l0", (L1, L0, True))
        | "--l1-vs-l1" `elem` args = ("--l1-vs-l1", (L1, L1, False))
        | "--l2-vs-l0" `elem` args = ("--l2-vs-l0", (L2, L0, True))
        | "--l2-vs-l1" `elem` args = ("--l2-vs-l1", (L2, L1, False))
        | "--l2-vs-l2" `elem` args = ("--l2-vs-l2", (L2, L2, False))
        | otherwise = ("--l1-vs-l0", (L1, L0, True))

  let (flag, (pt0, pt1, vsFixed)) = mode
  putStrLn $ "=== " ++ flag ++ " ==="
  putStrLn $ "SB: " ++ show pt0 ++ " | BB: " ++ show pt1 ++ " | Hands: " ++ show nHands
  putStrLn ""

  if vsFixed
        then do
          putStrLn "hand | SB-profit | SB-strategy"
          putStrLn "-----|-----------|-----------"
          let results = evalState (runVsFixed hvs o2 pt0 seat1Baseline nHands priorStr) (mkStdGen 7)
          forM_ results $ \(i, profit, s0) ->
            when (i <= 10 || i `mod` 20 == 0) $
              putStrLn $ show i ++ " | " ++ show profit ++ " | " ++ compactStrategyLine s0
          let total = sum [p | (_, p, _) <- results]
          putStrLn $ "\nTotal SB profit: " ++ show total ++ " (" ++ show (total / fromIntegral nHands) ++ "/hand)"
        else do
          putStrLn "hand | SB-profit | SB-strategy"
          putStrLn "-----|-----------|-----------"
          let results = evalState (runMatchup hvs o2 pt0 pt1 nHands priorStr) (mkStdGen 7)
          forM_ results $ \(i, profit, s0, _s1) ->
            when (i <= 10 || i `mod` 20 == 0) $
              putStrLn $ show i ++ " | " ++ show profit ++ " | " ++ compactStrategyLine s0
          let total = sum [p | (_, p, _, _) <- results]
          putStrLn $ "\nTotal SB profit: " ++ show total ++ " (" ++ show (total / fromIntegral nHands) ++ "/hand)"
