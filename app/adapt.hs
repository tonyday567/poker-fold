{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (when)
import Control.Monad.State.Lazy (State, evalState)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Poker.HandRank (hvs7)
import Poker.Random (dealTable)
import Poker.Range (Range, readSomeRanges)
import Poker.SimpleBelief
import Poker.Strategy
import Poker.Table
import System.Random (RandomGen, mkStdGen)
import Prelude

heroSeat :: Int
heroSeat = 0

oppSeat :: Int
oppSeat = 1

data HandResult = HandResult
  { finalTable :: Table,
    oppObs :: [Obs],
    heroProfit :: Double
  }
  deriving (Eq, Show)

-- | Play a single preflop hand. The hero uses the provided strategy; the
-- opponent uses their fixed true strategy.
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

-- | Run repeated hands, updating the lightweight action-frequency belief after
-- each observed opponent action. The hero acts from the current belief.
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

main :: IO ()
main = do
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"
  let trueOpp = seat1Baseline
  let belief0 = initialBelief seat1Baseline
  putStrLn $ "Baseline profile: " ++ compactProfile seat0Baseline seat1Baseline
  let results = evalState (runHands hvs o2 trueOpp belief0 200) (mkStdGen 7)
  printSummary results
