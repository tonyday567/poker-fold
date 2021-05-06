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
import qualified Data.Poker as Poker
import qualified Data.Poker.Deck as Deck
import qualified Prelude as P
import Chart (fixed)
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Discrimination as D
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- https://math.stackexchange.com/questions/1363239/fast-way-to-get-a-position-of-combination-without-repetitions
-- https://math.stackexchange.com/questions/1368526/fast-way-to-get-a-combination-given-its-position-in-reverse-lexicographic-or/1368570#1368570


-- * random card generation

-- | random 7-Card vectors
--
card7s :: Int -> [[Card]]
card7s n = evalState (replicateM n (dealN 7)) (mkStdGen 42)

-- * sorting

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
-- > shuffle 52 (take 52 rvs52) == ishuffle rvs52
ishuffle' :: [Int] -> [Int]
ishuffle' as = Set.toAscList $ go as Set.empty
  where
    go [] s = s
    go (x0 : xs) s = go xs (Set.insert x1 s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + one) (d <= acc)) x0 s

dealN' :: (RandomGen g) => Int -> State g [Card]
dealN' n = fmap toEnum . ishuffle' <$> rvis 52 n

card7s' :: Int -> [[Card]]
card7s' n = evalState (replicateM n (dealN' 7)) (mkStdGen 42)

--
-- >>> S.length $ card7sS 100
-- 700
card7sS :: Int -> S.Vector Int
card7sS n = S.fromList $ mconcat $ evalState (replicateM n (fmap fromEnum <$> dealN' 7)) (mkStdGen 42)

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

    -- 2.188
    "poker-eval" ->
      logTick "pokerEval"
      (fmap (Deck.unNumericalHandValue . Poker.numericalHandValue_n 7))
      (fmap
       (Poker.fromList .
        fmap (\(Card x y) ->
               Poker.mkCard (toEnum . fromEnum $ x) (toEnum . fromEnum $ y)))
       (card7s n))

    -- 0.455
    "lookupHRs" -> do
      s <- hvs7
      let !cs = card7sS n
      logTick "lookupHRs"
        (lookupHRs s) cs

    -- 0.252
    "lookupHRsPos" -> do
      let !cs = card7sS n
      logTick "lookupHRs: Pos Calculation Component"
        (\n' -> S.generate n' (\x -> toLexiPosRS 52 7 (S.slice (x*7) 7 cs)))
        n

    -- 0.251
    "lookupHRsLookup" -> do
      let !cs = card7sS n
      logTick "lookupHRs: Lookup Component"
        (\n' -> S.generate n' (\x -> toLexiPosRS 52 7 (S.slice (x*7) 7 cs)))
        n

    -- 2.651
    "lookupHR" -> do
      s <- hvs7
      logTick "lookupHR"
        (fmap (lookupHR s))
        (S.fromList . fmap fromEnum <$> card7s n)

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
    -- 1.899
    "toLexiPosR" -> do
      let !ts = evalState (replicateM n (fmap fromEnum <$> dealN' 7)) (mkStdGen 42)
      logTick "toLexiPosR" (fmap (toLexiPosR 52 7)) ts
    -- 0.250
    "toLexiPosRS" -> do
      let !ts = S.fromList $ mconcat $ evalState (replicateM n (fmap fromEnum <$> dealN' 7)) (mkStdGen 42)
      logTick "toLexiPosRS" (\ts' -> S.generate n (\x -> toLexiPosRS 52 7 (S.slice (x*7) 7 ts'))) ts
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
    "rvi" -> logTick "rvi * 7" (\x -> evalState (replicateM (x * 7) (rvi 52)) (mkStdGen 42)) n
    "card7s" -> logTick "card7s" card7s n
    "card7sS" -> logTick "card7sS" card7sS n
    "card7sSet" -> logTick "card7s set" card7s' n
    "sort" -> logTick "list sort" (fmap (sortOn Down)) (card7s n)
    "sortd" -> logTick "discrimination sort" (fmap (D.sortWith id)) (fmap fromEnum <$> card7s n)
    "sortseq" -> logTick "sequence sort" (fmap (Seq.sortOn Down)) (Seq.fromList <$> card7s n)
    _ -> pure ()

