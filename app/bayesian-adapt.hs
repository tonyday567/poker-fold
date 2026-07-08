{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.State.Lazy (State, evalState)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Poker.HandRank (hvs7)
import Poker.Random (dealTable)
import Poker.Range (Range (..), readSomeRanges)
import Poker.Strategy
import Poker.Table hiding (fromRawAction)
import System.Environment (getArgs)
import System.Random (RandomGen, mkStdGen)
import Prelude

shiftStrategy :: Double -> Strategy -> Strategy
shiftStrategy delta (Strategy ts) =
  Strategy [sort4 [clamp (shift t) | t <- [t0,t1,t2,t3]] | (t0,t1,t2,t3) <- ts]
  where
    clamp x = max 0.0 (min 1.0 x)
    shift t | delta >= 0 = t + delta * (1 - t)
            | otherwise  = t * (1 + delta)
    sort4 [a,b,c,d] = case List.sort [a,b,c,d] of [x,y,z,w] -> (x,y,z,w); _ -> error "sort4"

handComboWeight :: Int -> Double
handComboWeight hc = let (d,m) = hc `divMod` 13 in if d==m then 6 else if d<m then 12 else 4

centroidActionProb :: Strategy -> Range Double -> Int -> RawAction -> Double
centroidActionProb strat@(Strategy ts) o2 ctx raw =
  let o2list = toList o2
      tup = ts !! ctx
      legal = contextLegalActions ctx
      totalW = sum [handComboWeight hc | hc <- [0..168]]
      chooseActionHC hc =
        let e = o2list !! hc
            intended = intendedAction tup e
            a = actionAggression intended
            legalAs = [(actionAggression (fromRawAction r), r) | r <- legal]
            clamped = snd $ List.minimumBy (compare `on` (\(a', _) -> (abs (a' - a), a'))) legalAs
         in clamped
      actionCount = sum [ handComboWeight hc | hc <- [0..168], chooseActionHC hc == raw ]
   in if totalW <= 0 then 0 else actionCount / totalW

bayesianUpdate :: Bool -> [Double] -> [Strategy] -> Range Double -> Int -> RawAction -> [Double]
bayesianUpdate smoothed prior strats o2 ctx raw =
  let likelihoods = [centroidActionProb s o2 ctx raw | s <- strats]
      unnorm = zipWith (*) prior likelihoods
      total = sum unnorm
      posterior = if total <= 0 then prior else map (/ total) unnorm
   in if smoothed
        then let eps = 0.01
                 smoothed' = map (+ eps) posterior
                 stotal = sum smoothed'
              in map (/ stotal) smoothed'
        else posterior

-- ============================================================================
-- Level 2 Deception
-- ============================================================================

-- | BB adapts strategy based on belief about SB: if BB thinks SB is tight,
-- BB loosens up (negative shift). If BB thinks SB is loose, BB tightens.
bbStrategyFromBelief :: [Double] -> [Double] -> Strategy
bbStrategyFromBelief belief centroidDeltas =
  let effectiveDelta = -(sum (zipWith (*) belief centroidDeltas))
   in shiftStrategy effectiveDelta seat1Baseline

-- | Play one hand where BB's strategy depends on BB's belief about SB.
-- Both players observe each other (SB observes BB, BB observes SB).
playHandDeceive ::
  S.Vector Word16 -> Range Double -> Strategy -> [Double] -> [Strategy] ->
  Table -> ([Obs], [Obs], Double)
playHandDeceive hvs o2 sbStrat bbBelief sbCentroids t0 = go t0 [] []
  where
    bbStrat = bbStrategyFromBelief bbBelief [-0.25, 0, 0.25]
    go t sbObs bbObs = case cursor t of
      Nothing ->
        let t' = showdown hvs t
            profit = stacks t' !! 0 - 10.0
         in (sbObs, bbObs, profit)
      Just p ->
        let ctx = fromMaybe (error "playHandDeceive: no context") (decisionContext t)
            hole = playerCards (cards t) !! p
            raw = strategyAction o2 (if p == 0 then sbStrat else bbStrat) ctx hole
            t' = actOn raw t
            sbObs' = if p == 1 then Obs ctx hole raw : sbObs else sbObs
            bbObs' = if p == 0 then Obs ctx hole raw : bbObs else bbObs
         in go t' sbObs' bbObs'

main :: IO ()
main = do
  args <- getArgs
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"

  -- SB centroids (what BB thinks about SB)
  let sbCentroids = [shiftStrategy (-0.25) seat0Baseline, seat0Baseline, shiftStrategy 0.25 seat0Baseline]
      sbLabels = ["loose-SB", "base-SB", "tight-SB"]
      sbDeltas = [-0.25, 0, 0.25]

  -- SB's true strategies
  let sbDecoy = sbCentroids !! 2   -- tight (deceive)
      sbTrue  = sbCentroids !! 0   -- loose (true style)

  if "--deceive" `elem` args
    then do
      let decoyHands = 20
          totalHands = 100
          nCentroids = 3
          prior0 = replicate nCentroids (1.0 / fromIntegral nCentroids)

      putStrLn "=== Level 2 Deception ==="
      putStrLn $ "SB plays tight for " ++ show decoyHands ++ " hands, then switches to loose"
      putStrLn $ "BB updates belief about SB and adjusts strategy"
      putStrLn ""

      let results = evalState (go 1 prior0 0 []) (mkStdGen 7)
          go i bbBelief cumProfit acc
            | i > totalHands = pure (reverse acc)
            | otherwise = do
                let sbStrat = if i <= decoyHands then sbDecoy else sbTrue
                t0 <- dealTable defaultTableConfig
                let (sbObs, bbObs, profit) = playHandDeceive hvs o2 sbStrat bbBelief sbCentroids t0
                    -- BB updates belief about SB from what BB observes (SB's actions)
                    bbBelief' = foldl (\b (Obs ctx _ raw) -> bayesianUpdate True b sbCentroids o2 ctx raw) bbBelief bbObs
                    cum' = cumProfit + profit
                go (i+1) bbBelief' cum' ((i, profit, cum', bbBelief') : acc)

      putStrLn "hand | phase   | profit | BB-thinks-SB-is"
      putStrLn "-----|---------|--------|--------------"
      forM_ results $ \(i, profit, _cum, bbBel) ->
        when (i <= 10 || i == decoyHands || i == decoyHands+1 || i `mod` 20 == 0) $ do
          let phase = if i <= decoyHands then "decoy" else "exploit" :: String
              best = fst $ List.maximumBy (compare `on` snd) (zip sbLabels bbBel)
          putStrLn $ show i ++ " | " ++ phase ++ " | " ++ show profit ++ " | " ++ best ++ "=" ++ take 5 (show (List.maximum bbBel))

      -- Separate EV by phase
      let (decoyResults, exploitResults) = List.partition (\(i,_,_,_) -> i <= decoyHands) results
          decoyEV = sum [p | (_,p,_,_) <- decoyResults]
          exploitEV = sum [p | (_,p,_,_) <- exploitResults]
      putStrLn ""
      putStrLn $ "Decoy phase (" ++ show decoyHands ++ " hands) SB profit: " ++ show decoyEV ++ " (" ++ show (decoyEV / fromIntegral decoyHands) ++ "/hand)"
      putStrLn $ "Exploit phase (" ++ show (totalHands - decoyHands) ++ " hands) SB profit: " ++ show exploitEV ++ " (" ++ show (exploitEV / fromIntegral (totalHands - decoyHands)) ++ "/hand)"

    else do
      -- Original Level 1 mode
      let ctStrats = [shiftStrategy (-0.25) seat1Baseline, seat1Baseline, shiftStrategy 0.25 seat1Baseline]
          ctLabels = ["loose", "baseline", "tight"]
          nHands = 200
          nCentroids = 3
          oppLabel = case argValue "--opponent" args of
                       Just l | l `elem` ctLabels -> l
                       _ -> "loose"
          Just oppIdx = List.elemIndex oppLabel ctLabels
          opp = ctStrats !! oppIdx
          hero = seat0Baseline

      putStrLn $ "=== Bayesian Belief Tracking ==="
      putStrLn $ "Opponent: " ++ oppLabel
      putStrLn $ "Hands: " ++ show nHands

      let prior0 = replicate nCentroids (1.0 / fromIntegral nCentroids)
          results = evalState (go 1 prior0 0 []) (mkStdGen 7)
          go i bel cumProfit acc
            | i > nHands = pure (reverse acc)
            | otherwise = do
                t0 <- dealTable defaultTableConfig
                let (obs, profit) = playHandFixed hvs o2 hero opp t0
                    bel' = foldl (\b (Obs ctx _ raw) -> bayesianUpdate False b ctStrats o2 ctx raw) bel obs
                    cum' = cumProfit + profit
                go (i+1) bel' cum' ((i, profit, cum', bel') : acc)

      putStrLn "hand | profit | belief"
      putStrLn "-----|--------|-------"
      forM_ results $ \(i, profit, _cum, bel) ->
        when (i <= 10 || i `mod` 20 == 0) $
          putStrLn $ show i ++ " | " ++ show profit ++ " | " ++
            unwords [l ++ "=" ++ take 5 (show (bel !! k)) | (k,l) <- zip [0..] ctLabels]

      case reverse results of
        [] -> return ()
        (_, _, total, finalBel) : _ -> do
          putStrLn ""
          putStrLn $ "Final belief: " ++ unwords [l ++ "=" ++ show (finalBel !! k) | (k,l) <- zip [0..] ctLabels]
          putStrLn $ "Cumulative SB profit: " ++ show total ++ " (" ++ show (total / fromIntegral nHands) ++ "/hand)"

-- Helpers
playHandFixed :: S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Table -> ([Obs], Double)
playHandFixed hvs o2 hero opp t0 = go t0 []
  where
    go t obs = case cursor t of
      Nothing -> let t' = showdown hvs t; profit = stacks t' !! 0 - 10.0 in (obs, profit)
      Just p ->
        let ctx = fromMaybe (error "playHandFixed: no context") (decisionContext t)
            hole = playerCards (cards t) !! p
            raw = strategyAction o2 (if p == 0 then hero else opp) ctx hole
            t' = actOn raw t
            obs' = if p == 1 then Obs ctx hole raw : obs else obs
         in go t' obs'

argValue :: String -> [String] -> Maybe String
argValue _ [] = Nothing
argValue key (k:v:rest) | k == key = Just v
argValue key (_:rest) = argValue key rest
