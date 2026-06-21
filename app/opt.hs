{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.State.Lazy (evalState)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as S
import Data.Word (Word16)
import Poker.HandRank (hvs7)
import Poker.Range (Range, readSomeRanges)
import Poker.Strategy
import System.Random (RandomGen, mkStdGen, split)
import Prelude

namedStrategies :: [(String, Strategy)]
namedStrategies =
  [ ("alwaysFold", Strategy [(1.0, 1.0, 1.0, 1.0), (1.0, 1.0, 1.0, 1.0), (1.0, 1.0, 1.0, 1.0), (1.0, 1.0, 1.0, 1.0), (1.0, 1.0, 1.0, 1.0)]),
    ("alwaysCall", Strategy [(0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0)]),
    ("alwaysAllIn", Strategy [(0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0), (0.0, 1.0, 1.0, 1.0)]),
    ( "tightAllIn",
      Strategy
        [ (0.25, 0.75, 0.90, 0.95),
          (0.10, 0.50, 0.90, 0.95),
          (0.25, 0.75, 1.0, 1.0),
          (0.10, 0.50, 1.0, 1.0),
          (0.25, 0.75, 1.0, 1.0)
        ]
    )
  ]

runEv :: S.Vector Word16 -> Range Double -> Strategy -> Strategy -> Int -> Double
runEv hvs o2 hero opp n = evalState (evStrategy hvs o2 hero opp n) (mkStdGen 123)

printEvs :: S.Vector Word16 -> Range Double -> Strategy -> IO ()
printEvs hvs o2 s = do
  let vsSelf = runEv hvs o2 s s 1000
  putStrLn $ "  vs self: " ++ show vsSelf

randomStrategies :: (RandomGen g) => Int -> g -> [Strategy]
randomStrategies 0 _ = []
randomStrategies n g =
  let (g1, g2) = split g
   in randomStrategy g1 : randomStrategies (n - 1) g2

randomSearchSelf :: S.Vector Word16 -> Range Double -> Int -> Int -> Strategy
randomSearchSelf hvs o2 nSamples nSims =
  let samples = randomStrategies nSamples (mkStdGen 42)
      score s = runEv hvs o2 s s nSims
   in snd $ maximumBy (compare `on` fst) [(score s, s) | s <- samples]

randomSearchOpp :: S.Vector Word16 -> Range Double -> Strategy -> Int -> Int -> Strategy
randomSearchOpp hvs o2 opp nSamples nSims =
  let samples = randomStrategies nSamples (mkStdGen 42)
      score s = runEv hvs o2 s opp nSims
   in snd $ maximumBy (compare `on` fst) [(score s, s) | s <- samples]

bestResponseSeat0 :: S.Vector Word16 -> Range Double -> Strategy -> Int -> Int -> Strategy
bestResponseSeat0 hvs o2 opp nSamples nSims =
  let samples = randomStrategies nSamples (mkStdGen 42)
      score s = runEv hvs o2 s opp nSims
   in snd $ maximumBy (compare `on` fst) [(score s, s) | s <- samples]

bestResponseSeat1 :: S.Vector Word16 -> Range Double -> Strategy -> Int -> Int -> Strategy
bestResponseSeat1 hvs o2 hero nSamples nSims =
  let samples = randomStrategies nSamples (mkStdGen 7)
      score s = runEv hvs o2 hero s nSims
   in snd $ minimumBy (compare `on` fst) [(score s, s) | s <- samples]

main :: IO ()
main = do
  hvs <- hvs7
  ranges <- fromMaybe (error "could not read some.str") <$> readSomeRanges
  let o2 = ranges Map.! "o2"

  putStrLn "Named strategies vs themselves:"
  mapM_ (\(n, s) -> putStr (n ++ ":") >> printEvs hvs o2 s) namedStrategies

  putStrLn "\nRandom-search best symmetric strategy (vs self):"
  let bestSelf = randomSearchSelf hvs o2 500 300
  putStrLn $ "best: " ++ show bestSelf
  putStrLn $ "EV vs self: " ++ show (runEv hvs o2 bestSelf bestSelf 2000)

  putStrLn "\nRandom-search best response to alwaysCall:"
  let oppCall = fromMaybe (error "alwaysCall") $ lookup "alwaysCall" namedStrategies
  let bestVsCall = randomSearchOpp hvs o2 oppCall 500 300
  putStrLn $ "best: " ++ show bestVsCall
  putStrLn $ "EV vs alwaysCall: " ++ show (runEv hvs o2 bestVsCall oppCall 2000)

  putStrLn "\nBest-response check for best symmetric strategy:"
  let brBestSelf = randomSearchOpp hvs o2 bestSelf 500 300
  putStrLn $ "BR EV vs bestSelf: " ++ show (runEv hvs o2 brBestSelf bestSelf 2000)

  putStrLn "\nIterative best-response (seat0 / seat1):"
  let start = fromMaybe (error "start") $ lookup "alwaysCall" namedStrategies
      nSamp = 1000
      nSim = 500
      nFinal = 5000
      go :: Int -> Strategy -> Strategy -> IO ()
      go i s0 s1
        | i > 6 = do
            putStrLn $ "Final seat0 strategy: " ++ compactStrategy s0
            putStrLn $ "Final seat1 strategy: " ++ compactStrategy s1
            putStrLn $ "EV(s0f, s1f): " ++ show (runEv hvs o2 s0 s1 nFinal)
        | otherwise = do
            let s1' = bestResponseSeat1 hvs o2 s0 nSamp nSim
                s0' = bestResponseSeat0 hvs o2 s1' nSamp nSim
            putStrLn $ "iter " ++ show i ++ " EV(s0,s1): " ++ show (runEv hvs o2 s0' s1' 1000)
            go (i + 1) s0' s1'
  go 1 start (bestResponseSeat1 hvs o2 start nSamp nSim)
