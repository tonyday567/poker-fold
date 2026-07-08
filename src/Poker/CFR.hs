{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Poker.CFR
  ( ActionIdx (..),
    contextActions,
    nActions,
    StrategyTable,
    RegretTable,
    emptyStrategy,
    emptyRegrets,
    uniformStrategy,
    stratProb,
    cfrIterate,
    runCFR,
    exploitability,
    handComboWeight,
  )
where

import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Functor.Rep (index)
import Data.List qualified as List
import Optics.Core (review)
import Poker.EquityMatrix (EqMatrix (..), ix, disjointHoles)
import Poker.Range (Range, StartingHandS (..), startingHandI, toRepHole)
import Prelude

data ActionIdx = AFold | ACall | ARaise | AAllIn
  deriving (Eq, Ord, Show, Enum, Bounded)

contextActions :: Int -> [ActionIdx]
contextActions = \case
  0 -> [AFold, ACall, ARaise, AAllIn]
  1 -> [ACall, ARaise, AAllIn]
  2 -> [AFold, ACall, AAllIn]
  3 -> [AFold, ACall]
  4 -> [AFold, ACall, AAllIn]
  5 -> [AFold, ACall]
  _ -> error "bad context"

nActions :: Int -> Int
nActions = length . contextActions

nHandClasses, nContexts :: Int
nHandClasses = 169
nContexts = 6

type StrategyTable = [[Double]]
type RegretTable = [[Double]]

emptyStrategy, uniformStrategy :: StrategyTable
emptyStrategy = concat [replicate nHandClasses (replicate (nActions c) 0.0) | c <- [0..nContexts-1]]
uniformStrategy = concat [replicate nHandClasses (replicate (nActions c) (1 / fromIntegral (nActions c))) | c <- [0..nContexts-1]]

emptyRegrets :: RegretTable
emptyRegrets = emptyStrategy

stratProb :: StrategyTable -> Int -> Int -> ActionIdx -> Double
stratProb ss ctx hc a =
  let i = ctx * nHandClasses + hc
      acts = contextActions ctx
   in if i < 0 || i >= length ss
        then 0.0
        else case List.elemIndex a acts of
               Just j | j < length (ss !! i) -> ss !! i !! j
               _ -> 0.0

handComboWeight :: Int -> Double
handComboWeight hc = let (d,m)=hc`divMod`13 in if d==m then 6 else if d<m then 12 else 4

regretMatch :: [Double] -> [Double]
regretMatch rs = let pos=map(max 0)rs; t=sum pos
                  in if t<=0 then replicate(length rs)(1/fromIntegral(length rs)) else map(/t)pos

updateStrategy :: RegretTable -> StrategyTable
updateStrategy rs = map regretMatch rs

-------------------------------------------------------------------------------
-- Game tree (precomputed structure)
-------------------------------------------------------------------------------

-- | Which player acts at each context: 0=SB, 1=BB.
whoActs :: Int -> Int
whoActs = \case 0->0; 1->1; 2->1; 3->1; 4->0; 5->0; _->error "whoActs"

-- | Transition: (context, action) → next state.
data Next = NFold | NShowdown | NCont Int
  deriving (Eq, Show)

nextState :: Int -> ActionIdx -> Next
nextState ctx a = case (ctx,a) of
  (0,AFold)->NFold; (0,ACall)->NCont 1; (0,ARaise)->NCont 2; (0,AAllIn)->NCont 3
  (1,ACall)->NShowdown; (1,ARaise)->NCont 4; (1,AAllIn)->NCont 5
  (2,AFold)->NFold; (2,ACall)->NShowdown; (2,AAllIn)->NCont 5
  (3,AFold)->NFold; (3,ACall)->NShowdown
  (4,AFold)->NFold; (4,ACall)->NShowdown; (4,AAllIn)->NCont 3
  (5,AFold)->NFold; (5,ACall)->NShowdown
  _ -> error "bad nextState"

-- | SB's total investment and total pot size at each (context, action).
investedPot :: Int -> ActionIdx -> (Double, Double)
investedPot ctx a = case (ctx,a) of
  (0,AFold)->(0.5,1.5); (0,ACall)->(1.0,2.0); (0,ARaise)->(3.0,4.0); (0,AAllIn)->(10.0,11.0)
  (1,ACall)->(1.0,2.0); (1,ARaise)->(1.0,4.0); (1,AAllIn)->(1.0,11.0)
  (2,AFold)->(3.0,0.0); (2,ACall)->(3.0,6.0); (2,AAllIn)->(3.0,13.0)
  (3,AFold)->(10.0,0.0); (3,ACall)->(10.0,20.0)
  (4,AFold)->(1.0,0.0); (4,ACall)->(3.0,6.0); (4,AAllIn)->(10.0,13.0)
  (5,AFold)->(1.0,0.0); (5,ACall)->(10.0,20.0)
  _ -> error "bad investedPot"

-- | SB's profit when the hand ends with a fold at (context, action).
foldProfit :: Int -> ActionIdx -> Double
foldProfit ctx a = case (ctx,a) of
  (0,AFold) -> -0.5   -- SB folds preflop, loses small blind
  (2,AFold) -> 1.0    -- BB folds to SB's 3bb raise
  (3,AFold) -> 1.0    -- BB folds to SB's all-in
  (4,AFold) -> -1.0   -- SB folds to BB's raise
  (5,AFold) -> -1.0   -- SB folds to BB's all-in
  _ -> error "foldProfit: not a fold context"

-------------------------------------------------------------------------------
-- Bottom-up action value computation
-------------------------------------------------------------------------------

-- | Compute action values bottom-up. Order: [5,3,4,2,1,0].
computeActionValues :: EqMatrix -> StrategyTable -> [[Double]]
computeActionValues eq st = go [] [5,3,4,2,1,0]
  where
    go acc [] = acc
    go acc (ctx:rest) =
      let ctxVals =
            [ [ actionValue eq st restVals ctx hc a
              | a <- contextActions ctx
              ]
            | hc <- [0 .. nHandClasses - 1]
            ]
          restVals = acc
       in go (ctxVals ++ acc) rest

    -- Value of taking action a at context ctx with hand hc.
    actionValue eq st restVals ctx hc a =
      case nextState ctx a of
        NFold -> foldProfit ctx a
        NShowdown ->
          let (invested, pot) = investedPot ctx a
              -- Expected pot share over villain hands
              villainHands =
                [ vi
                | vi <- [0 .. nHandClasses - 1],
                  disjointHoles
                    (toRepHole (review startingHandI (StartingHandS (fromIntegral hc))))
                    (toRepHole (review startingHandI (StartingHandS (fromIntegral vi))))
                ]
              villainWeights = [handComboWeight vi | vi <- villainHands]
              totalW = sum villainWeights
           in if totalW <= 0
                then -invested
                else -invested
                       + pot
                       * sum [handComboWeight vi * ix eq hc vi | vi <- villainHands]
                       / totalW
        NCont nextCtx ->
          let (invested, _) = investedPot ctx a
              -- Who acts at nextCtx?
              -- If nextCtx is OUR turn: value = Σ_a' prob(a'|hc) * value[nextCtx][hc][a']
              -- If nextCtx is OPPONENT's turn: value = Σ_v prob(v|hc) Σ_a' prob(a'|v) * value[nextCtx][v][a']
              ourTurn = whoActs nextCtx == whoActs ctx
           in if ourTurn
                then
                  let acts = contextActions nextCtx
                      probs = [stratProb st nextCtx hc a' | a' <- acts]
                      -- Find nextCtx values in restVals (they're already computed since we go bottom-up)
                      nextVals = lookupCtxVals restVals nextCtx hc
                   in -invested + sum (zipWith (*) probs nextVals)
                else
                  -- Opponent's turn: average over villain hands and opponent actions
                  let villainHands =
                        [ vi
                        | vi <- [0 .. nHandClasses - 1],
                          disjointHoles
                            (toRepHole (review startingHandI (StartingHandS (fromIntegral hc))))
                            (toRepHole (review startingHandI (StartingHandS (fromIntegral vi))))
                        ]
                      villainWeights = [handComboWeight vi | vi <- villainHands]
                      totalW = sum villainWeights
                      valForVillain vi =
                        let acts = contextActions nextCtx
                            probs = [stratProb st nextCtx vi a' | a' <- acts]
                            nextVals = lookupCtxVals restVals nextCtx vi
                         in negate (sum (zipWith (*) probs nextVals))
                   in if totalW <= 0
                        then -invested
                        else -invested
                               + sum [handComboWeight vi * valForVillain vi | vi <- villainHands]
                               / totalW

    -- Look up precomputed values for a context and hand class.
    -- restVals accumulates in reverse processing order: [latest, ..., earliest].
    -- Compute dynamic index from the fixed processing order [5,3,4,2,1,0].
    lookupCtxVals restVals ctx hc =
      let order = [5,3,4,2,1,0]
          -- Number of already-processed contexts
          k = length restVals `div` nHandClasses
          -- Contexts in restVals (most recently processed first)
          contexts = reverse (take k order)
          ctxIdx = case List.elemIndex ctx contexts of
            Just i -> i
            Nothing -> error $ "lookupCtxVals: context " ++ show ctx ++ " not in restVals (" ++ show contexts ++ ")"
       in restVals !! (ctxIdx * nHandClasses + hc)

-------------------------------------------------------------------------------
-- CFR iteration
-------------------------------------------------------------------------------

cfrIterate :: EqMatrix -> StrategyTable -> RegretTable -> IO (StrategyTable, RegretTable, Double)
cfrIterate eq st0 reg0 = do
  let allVals = computeActionValues eq st0
      newRows =
        [ let ctx = idx `div` nHandClasses
              hc = idx `mod` nHandClasses
              acts = contextActions ctx
              vals = lookupCtxVals' allVals ctx hc
              probs = [stratProb st0 ctx hc a | a <- acts]
              ev = sum (zipWith (*) probs vals)
              regrets = zipWith (\v _p -> v - ev) vals probs
           in zipWith (+) row regrets
        | (idx, row) <- zip [0..] reg0
        ]
      reg1 = newRows
      st1 = updateStrategy reg1
      ev = strategySelfEV eq st1 allVals
  pure (st1, reg1, ev)

lookupCtxVals' :: [[Double]] -> Int -> Int -> [Double]
lookupCtxVals' allVals ctx hc = allVals !! (ctx * nHandClasses + hc)

strategySelfEV :: EqMatrix -> StrategyTable -> [[Double]] -> Double
strategySelfEV _eq st allVals =
  let evs =
        [ let vals = lookupCtxVals' allVals 0 hc
              probs = [stratProb st 0 hc a | a <- contextActions 0]
           in sum (zipWith (*) probs vals) * handComboWeight hc
        | hc <- [0 .. nHandClasses - 1]
        ]
      totalW = sum [handComboWeight hc | hc <- [0 .. nHandClasses - 1]]
   in sum evs / totalW

runCFR :: EqMatrix -> Int -> IO (StrategyTable, [Double])
runCFR eq n = do
  putStrLn $ "CFR: " ++ show n ++ " iterations"
  go 1 uniformStrategy emptyRegrets []
  where
    go i st reg evs
      | i > n = pure (st, reverse evs)
      | otherwise = do
          (st', reg', ev) <- cfrIterate eq st reg
          when (i <= 5 || i `mod` 10 == 0) $
            putStrLn $ "  iter " ++ show i ++ "  self-EV: " ++ show ev
          go (i + 1) st' reg' (ev : evs)

exploitability :: EqMatrix -> StrategyTable -> IO Double
exploitability eq st = do
  let allVals = computeActionValues eq st
      ev = strategySelfEV eq st allVals
  putStrLn $ "Self-play EV: " ++ show ev ++ "  |EV| = " ++ show (abs ev)
  pure (abs ev)
