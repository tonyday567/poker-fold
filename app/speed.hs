{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import NumHask.Prelude
import Poker
import Poker.Random
import Perf
import qualified Prelude as P
import Chart (fixed)
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- * performance helpers
logTick :: (NFData b) => Text -> (a -> b) -> a -> IO ()
logTick l x y = do
  (t, _) <- tick x y
  putStrLn (l <> ": " <> fixed (Just 3) (toSecs t))

logTicks :: (NFData b) => Int -> Text -> (a -> b) -> a -> IO ()
logTicks n l x y = do
  (t, _) <- ticks n x y
  putStrLn (l <> ": " <> fixed (Just 3) (toSecs $ sum t))

toSecs :: Cycle -> Double
toSecs = (/ 2.2e9) . P.fromIntegral

main :: IO ()
main = do
  args' <- getArgs
  let (run, n) = case args' of
        [] -> ("lookupHRs", 1000000)
        (r':[]) -> (pack r', 1000000)
        (r':n':_) -> (pack r', fromMaybe 1000000 $ readMaybe n')
  handRankSpeeds n run
  binoms n run
  hrlookups n run
  lookupTech n run
  cardChecks n run
  winodds n run
  handRankComponents n run

-- FIXME: horrible perf
--
-- > stack exec speed winodds 10000
-- win odds: 7.387
winodds :: Int -> Text -> IO ()
winodds n run = case run of
    "winodds" -> do
      logTick "win odds" (toList . winOdds 2) n
    _ -> pure ()

binoms :: Int -> Text -> IO ()
binoms n run = case run of
    "binom" -> logTicks n "binom" (binom 52) 7
    "binomR" -> logTicks n "binomR" (binomR 52) 7
    "binomM" -> do
      -- http://hackage.haskell.org/package/chimera-0.2.0.0/docs/Data-Chimera.html
      logTicks n "binomM" (binomM 52) 7
    _ -> pure ()

handRankSpeeds :: Int -> Text -> IO ()
handRankSpeeds n run = case run of
    -- 5.45
    "handRank" -> logTick "handRank" (fmap handRank) (card7s n)

    -- 0.396
    -- toLexiPosRS: 0.267
    -- applyFlat: 0.010
    -- lookup: 0.087
    -- unknown: 0.032
    "lookupHRs" -> do
      s <- hvs7
      let !cs = card7sFlat n
      logTick "lookupHRs"
        (lookupHRs s) cs

    -- 0.359
    "lookupHR" -> do
      s <- hvs7
      let !cs = card7sFlat 1
      logTicks n "lookupHR"
        (lookupHR s)
        cs

    _ -> pure ()

-- | storable lookup components
lookupTech :: Int -> Text -> IO ()
lookupTech n run = case run of
    "read" -> do
      (t, _) <- tickIO hvs7
      putStrLn ("instantiate hvs7: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    "hvs7Write" -> do
      hvs7Write
    "combinations752" -> logTick "combinations 7 [0..51]: " (length . combinations 7) [0..51::Int]
    "allhandranks" -> do
      (t, _) <- tickIO $ pure allHandRanks
      putStrLn ("allHandRank instantiation: " <> fixed (Just 3) (toSecs t))

    -- 1.527
    "toLexiPosR" -> do
      let !ts = fmap fromEnum <$> card7s n
      logTick "toLexiPosR" (fmap (toLexiPosR 52 7)) ts

    -- 0.264
    -- applyFlat adds 0.0xx
    "toLexiPosRS" -> do
      let !cs = card7sFlat n
      logTick "toLexiPosRS" (applyFlat 7 (toLexiPosRS 52 7)) cs
    
    -- 0.087
    "lookupHRsLookup" -> do
      s <- hvs7
      let !cs = applyFlatS 7 (toLexiPosRS 52 7) (card7sFlat n)
      logTick "lookupHRs: Lookup Component"
        (S.map (s S.!))
        cs

    _ -> pure ()

hrlookups :: Int -> Text -> IO ()
hrlookups n t = case t of
    -- * lookups
    -- @7462 length, storable is the best lookup
    -- >>> stack exec speed storablemmaplookup
    -- storable mmap lookup: length: 7462: 0.013
    -- >>> stack exec speed storablelookup
    -- storable lookup: length: 7462: 0.003
    -- >>> stack exec speed vectorlookup
    -- vector lookup: length: 7462: 0.275
    -- >>> stack exec speed maplookup
    -- map lookup: length: 7462: 0.413
    "maplookup" -> do
      let !l = V.length allHandRanksV
      let !ts = evalState (replicateM n rvHandRank) (mkStdGen 42)
      logTick ("map lookup: length: " <> fixed (Just 0) (fromIntegral l)) (fmap ((Map.!) mapHRValue)) ts
    "storablemmaplookup" -> do
      s <- hvs7
      let !l = min 7462 (S.length s)
      let !rvs = evalState (S.replicateM n (rvi l)) (mkStdGen 42)
      logTick "storable mmap lookup: length: 7462" (S.map (s S.!)) rvs
    "vectorlookup" -> do
      let !l = V.length allHandRanksV
      let !rvs = evalState (replicateM n (rvi l)) (mkStdGen 42)
      logTick ("vector lookup: length: " <> fixed (Just 0) (fromIntegral l :: Double)) (fmap (allHandRanksV V.!)) rvs
    -- 0.003
    "storablelookup" -> do
      s <- hvs7
      let !s' = S.convert $ S.take 7462 s
      let !rvs = S.fromList $ evalState (replicateM n (rvi 7462)) (mkStdGen 42)
      logTick "storable lookup: length: 7462" (S.map (s' S.!)) rvs
    _ -> pure ()

cardChecks :: Int -> Text -> IO ()
cardChecks n t = case t of
    -- 0.824
    "rvi" -> logTick "rvi * 7" (\x -> evalState (replicateM (x * 7) (rvi 52)) (mkStdGen 42)) n
    -- 1.001
    "rvis" -> logTick "rvis" (\x -> evalState (replicateM x (rvis 52 7)) (mkStdGen 42)) n
    -- 1.027
    "rviv" -> logTick "rviv" (\x -> evalState (replicateM x (rviv 52 7)) (mkStdGen 42)) n
    -- 1.873
    "card7s" -> logTick "card7s" card7s n
    -- 2.505
    "card7sFlat" -> logTick "card7sFlat" card7sFlat n
    -- 1.001
    "card7sFlatI" -> logTick "card7sFlatI ishuffle" card7sFlatI n
    -- 2.273
    "cardSort" -> logTick "card list sort" (fmap (sortOn Down)) (card7s n)
    -- 2.667
    "cardSortSeq" -> logTick "sequence sort" (fmap (Seq.sortOn Down)) (Seq.fromList <$> card7s n)
    _ -> pure ()

handRankComponents :: Int -> Text -> IO ()
handRankComponents n run = case run of
    -- 4.297
    "handRankAll" -> logTick "handRank" (fmap handRank) (card7s n)

    -- 2.989
    "handRankS" -> logTick "handRankS" (applyFlatS 7 handRankS) (card7sFlat n)

    -- 0.284
    "rank" -> do
      let !cs = card7sFlat n
      logTick "rank for a flat"
        (applyFlat 7 (S.map (fromEnum . rank . toEnum))) cs

    -- 1.532
    "royals" -> do
      let !cs = card7sFlat n
      logTick "flush n straight check"
         (applyFlat 7
          (fmap fromEnum . (\x -> flush x <|> straight x) . fmap toEnum . S.toList))
         cs

    -- 0.612
    "flush" -> do
      let !cs = card7sFlat n
      logTick "flush"
         (applyFlat 7
          (fmap fromEnum . flush . fmap toEnum . S.toList))
         cs

    -- 1.082
    "straight" -> do
      let !cs = card7sFlat n
      logTick "straight"
         (applyFlat 7
          (fmap fromEnum . straight . fmap toEnum . S.toList))
         cs

    -- 1.507
    "kind" -> do
      let !cs = card7sFlat n
      logTick "kind"
         (applyFlat 7 (fromEnum . kind . fmap rank . fmap toEnum . S.toList))
         cs

    -- 0.707
    "kindS" -> do
      let !cs = card7sFlat n
      logTick "kindS"
         (applyFlat 7 kindS)
         cs

    -- 0.707
    "oRankCount" -> do
      let !cs = card7sFlat n
      logTick "oRankCount"
         (applyFlat 7 oRankCount)
         cs

    -- 1.887
    "rankCount" -> do
      let !cs = card7sFlat n
      logTick "rankCount"
         (applyFlat 7 (rankCount . fmap rank . fmap toEnum . S.toList))
         cs

    -- 0.307
    "rankCountS" -> do
      let !cs = card7sFlat n
      logTick "rankCountS"
         (applyFlat 7 rankCountS)
         cs

    -- 1.432
    "suitRanks" -> do
      let !cs = card7sFlat n
      logTick "suitRanks"
         (applyFlat 7 (suitRanks . fmap toEnum . S.toList))
         cs



    -- 0.873
    "handRankSort" -> do
      let !cs = V.convert $ card7sFlat n
      logTick "initial sort"
         (applyFlat 7 (sortOn Down . S.toList))
         cs

    -- 0.039
    "handRankSum" -> do
      let !cs = card7sFlat n
      logTick "sum"
         (applyFlatS 7 S.sum)
         cs

    -- 0.470
    "handRankToList" -> do
      let !cs = card7sFlat n
      logTick "toList"
         (applyFlat 7 S.toList)
         cs

    -- 0.206
    "wrapping" -> do
      let !cs = card7sFlat n
      logTick "wrapping"
         (applyFlat 7
          ((fromEnum :: Word16 -> Int) . sum . fmap (toEnum :: Int -> Word16) . S.toList))
         cs

    _ -> pure ()

