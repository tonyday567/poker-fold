{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import Poker hiding (fromList, Suited, Pair, Hand, Raise, Call, Fold, Seat)
import NumHask.Prelude
import Poker.Strategy
import Poker.Types
import Poker.Random
import Perf
import qualified Prelude as P
import Chart (fixed)
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Storable (Storable)
import Data.Text (Text, pack, unpack)
import Data.Word
import Data.List (sort)
import Text.Read (readMaybe)
import System.Environment
import Control.Monad
import Control.Monad.State.Lazy
import System.Random
import Control.Applicative

-- http://kcats.org/csci/464/doc/knuth/fascicles/fasc3a.pdf
-- https://www.daniweb.com/programming/computer-science/threads/41584/the-fastest-combination-generator
-- file:///Users/tonyday/Downloads/Loopless_Functional_Algorithms.pdf
-- https://hackage.haskell.org/package/combinat-0.2.10.0/docs/src/Math.Combinat.Numbers.Sequences.html
-- https://hackage.haskell.org/package/recursion-schemes-5.2.2.1/docs/Data-Functor-Foldable.html#g:2



{-
revolvingDoorOdd :: Int -> Int -> (M.Vector Int -> b) -> f b
revolvingDoorOdd n t f = do
  v <- M.generate t id
  go (n+1) 0 t
  where
    go u l j = do
      b <- f v
    r3 = do
      c1 <- M.read v (t - 1)
      c2 <- M.read v (t - 2)
      (bool (go u l j) (c1 + 1 < c2))
    c1 = M.read v (t - 1)
    c2 = M.read v (t - 2)

-}

{-
algoR :: Int -> Int -> [[a]]
algoR n k = undefined

-}

unfold1 :: (a -> Maybe a) -> a -> [a]
unfold1 f x = case f x of
  Nothing -> [x]
  Just y  -> x : unfold1 f y

-- | Generates all permutations of a multiset 
--   (based on \"algorithm L\" in Knuth; somewhat less efficient). 
--   The order is lexicographic.  
algoL :: (Ord a) => [a] -> [[a]] 
algoL xs' = unfold1 nextp xs' where

  -- next :: [a] -> Maybe [a]
  nextp xs = case findj (reverse xs,[]) of 
    Nothing -> Nothing
    Just ( (l:ls) , rs) -> Just $ inc l ls (reverse rs,[]) 
    Just ( [] , _ ) -> P.error "permute: should not happen"

  -- we use simple list zippers: (left,right)
  -- findj :: ([a],[a]) -> Maybe ([a],[a])   
  findj ( xxs@(x:xs) , yys@(y:_) ) = if x >= y 
    then findj ( xs , x : yys )
    else Just ( xxs , yys )
  findj ( x:xs , [] ) = findj ( xs , [x] )  
  findj ( [] , _ ) = Nothing
  
  -- inc :: a -> [a] -> ([a],[a]) -> [a]
  inc !u us ( (x:xs) , yys ) = if u >= x
    then inc u us ( xs , x : yys ) 
    else reverse (x:us)  ++ reverse (u:yys) ++ xs
  inc _ _ ( [] , _ ) = P.error "permute: should not happen"

sortS :: (Ord a, Storable a) => S.Vector a -> S.Vector a
sortS xs = S.create $ do
    xs' <- S.thaw xs
    Intro.sort xs'
    pure xs'

sortSNoinline :: (Ord a, Storable a) => S.Vector a -> S.Vector a
sortSNoinline xs = S.create $ do
    xs' <- S.thaw xs
    Intro.sort xs'
    pure xs'
{-# NOINLINE sortSNoinline #-}

-- * performance helpers
logTick :: Text -> (a -> b) -> a -> IO ()
logTick l x y = do
  (t, _) <- tick x y
  (putStrLn . unpack) (l <> ": " <> fixed (Just 3) (toSecs t))

logTickIO :: Text -> IO a -> IO ()
logTickIO l x = do
  (t, _) <- tickIO x
  (putStrLn . unpack) (l <> ": " <> fixed (Just 3) (toSecs t))

logTicks :: Int -> Text -> (a -> b) -> a -> IO ()
logTicks n l x y = do
  (t, _) <- ticks n x y
  (putStrLn . unpack) (l <> ": " <> fixed (Just 3) (toSecs $ sum t))

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
  resolution 2 n run
  writeSome n run
  sortingChecks n run
  lookupHR_Speeds n run
  algorithms n run

  case run of
    -- fromList [("flushS","0.566"),("kindS","0.546"),("ranksSet","0.978"),("straightS","0.222"),("toRanksS","0.102")]
    "handRankS_" -> do
      m <- handRankS_P n
      let m' = Map.map (\x -> fixed (Just 3) (toSecs x)) m
      print m'
    -- fromList [("flushS",1000000),("kindS",923093),("ranksSet",969338),("straightS",969338),("toRanksS",923093)]
    "handRankS_Count" -> do
      c <- handRankS_CountP n
      print c
    "lookupHR_" -> do
      s <- hvs7
      h <- lookupHR_ s (card7sS n)
      print (S.sum $ fst h)
      print $ Map.map (\x -> fixed (Just 3) (toSecs x)) (snd h)
    _ -> pure ()

sortingChecks :: Int -> Text -> IO ()
sortingChecks n t = do
  let cs = card7sS n
  case t of
    -- 1.699
    "listsort" -> logTick "listsort" (fmap sort) (liso cardsS7L cs)
    -- 0.912
    "listsortS" -> logTick "listsortS" (applyV (sort . S.toList . uncardsS)) cs
    -- 0.797
    -- sortS has deteriorated in performance, from 0.3, as I have used it.
    "sortS" -> logTick "sortS" (applyV (sortS . uncardsS)) cs
    -- 1.170
    "sortSNoinline" -> logTick "sortSNoinline" (applyV (sortSNoinline . uncardsS)) cs
    _ -> pure ()

algorithms :: Int -> Text -> IO ()
algorithms n t = do
  case t of
    --
    "algoL" -> do
      logTick "algorL" algoL [0 .. (n - 1)]
    _ -> pure ()


--
-- handRank evals are 169 * p * n
-- win odds: 8.758 for n = 10000 about 2.59 secs per million evals
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

-- | handRankS version for performance testing.
--
handRankS_ :: CardsS -> IO (HandRank, Map.Map Text Cycle)
handRankS_ cs = runPerfT $ do
  fl <- perf "flushS" cycles $ pure (flushS cs)
  case fl of
    Just x -> pure x
    Nothing -> do
      rs <- perf "ranksSet" cycles $ pure (ranksSet cs)
      st <- perf "straightS" cycles $ pure (straightS rs)
      case st of
        Just x -> pure x
        Nothing -> do
          rs2 <- perf "toRanksS" cycles $ pure (toRanksS cs)
          k <- perf "kindS" cycles $ pure (kindS rs2) 
          pure k

handRankS_Count :: CardsS -> IO (HandRank, Map.Map Text Int)
handRankS_Count cs = runPerfT $ do
  fl <- perf "flushS" count $ pure (flushS cs)
  case fl of
    Just x -> pure x
    Nothing -> do
      rs <- perf "ranksSet" count $ pure (ranksSet cs)
      st <- perf "straightS" count $ pure (straightS rs)
      case st of
        Just x -> pure x
        Nothing -> do
          rs2 <- perf "toRanksS" count $ pure (toRanksS cs)
          k <- perf "kindS" count $ pure (kindS rs2) 
          pure k


handRankS_P :: Int -> IO (Map Text Cycle)
handRankS_P n = do
      let cs = card7sS n
      rs <- V.sequence $ applyV handRankS_ cs
      pure $ Map.unionsWith (+) $ V.toList $ V.map snd rs

handRankS_CountP :: Int -> IO (Map Text Int)
handRankS_CountP n = do
      let cs = card7sS n
      rs <- V.sequence $ applyV handRankS_Count cs
      pure $ Map.unionsWith (+) $ V.toList $ V.map snd rs

handRankSpeeds :: Int -> Text -> IO ()
handRankSpeeds n run = case run of
    -- 5.45
    "handRank" -> logTick "handRank" (fmap handRank) (card7s n)

    -- 2.162
    "handRankS" -> logTick "handRankS" (applyV handRankS) (card7sS n)

    -- 1.938
    "handRankS2" -> logTick "handRankS Storable result" (applyS (fromEnum . handRankS)) (card7sS n)

    -- 0.396
    -- toLexiPosRS: 0.267
    -- applyFlat: 0.010
    -- lookup: 0.087
    -- unknown: 0.032
    "lookupHRUnsafe" ->
      logTick "lookupHRUnsafe" (applyV lookupHRUnsafe)
      (V.foldMap id $
       applyV (Cards2S . S.fromList . sort . S.toList . uncardsS)
       (card7sS n))

    -- 0.359
    "lookupHR2" -> do
      s <- hvs7
      let !cs = CardsS $ uncards2S (card7sS 1)
      logTicks n "lookupHR2"
        (lookupHR s)
        cs

    _ -> pure ()


-- * lookupHR performance
--
lookupHR_ :: S.Vector Word16 -> Cards2S -> IO (S.Vector Word16, Map.Map Text Cycle)
lookupHR_ s cs = runPerfT $ do
  cs' <- perf "sortS" cycles $ pure (applyM (sortS . uncardsS) cs)
  cs'' <- perf "fromEnum" cycles $ pure (S.map fromEnum cs')
  cs''' <- perf "toLexiPosRS" cycles $ pure (applyFlatS 7 (toLexiPosRS 52 7) cs'')
  cs'''' <- perf "S.!" cycles $ pure (S.map (s S.!) cs''')
  pure cs''''

-- * lookupHR component performance
--
lookupHR_Speeds :: Int -> Text -> IO ()
lookupHR_Speeds n run =
  case run of
    --
    "lookupHR_sortS" -> do
      logTicks n "lookupHR_SortS"
        (applyM (sortS . uncardsS))
        (card7sS n)
    --
    "lookupHR_fromEnum" -> do
      logTicks n "lookupHR_fromEnum"
        (S.map fromEnum)
        (applyM (sortS . uncardsS) (card7sS n))

    --
    "lookupHR_toLexiPosRS" -> do
      logTicks n "lookupHR_toLexiPosRS"
        (applyFlatS 7 (toLexiPosRS 52 7))
        (S.map fromEnum . applyM (sortS . uncardsS) $ (card7sS n))

    --
    "lookupHR_lookup" -> do
      s <- hvs7
      let !cs = (applyFlatS 7 (toLexiPosRS 52 7)) . S.map fromEnum . applyM (sortS . uncardsS) $ (card7sS n)
      logTicks n "lookupHR_lookup"
        (S.map (s S.!))
        cs

    _ -> pure ()


-- | storable lookup components
lookupTech :: Int -> Text -> IO ()
lookupTech n run = case run of
    "read" -> do
      (t, _) <- tickIO hvs7
      putStrLn ("instantiate hvs7: " <> show n <> " " <> unpack (fixed (Just 3) (toSecs t)))
    "hvs7Write" -> do
      hvs7Write
    "combinations752" -> logTick "combinations 7 [0..51]: " (length . combinations 7) [0..51::Int]
    "allhandranks" -> do
      (t, _) <- tickIO $ pure allHandRanks
      putStrLn ("allHandRank instantiation: " <> unpack (fixed (Just 3) (toSecs t)))

    -- 0.264
    -- applyFlat adds 0.0xx
    "toLexiPosRS" -> do
      let !cs = S.map fromEnum $ uncards2S $ card7sS n
      logTick "toLexiPosRS" (applyFlat 7 (toLexiPosRS 52 7)) cs

    -- 0.087
    "lookupHRsLookup" -> do
      s <- hvs7
      let !cs = applyFlatS 7 (toLexiPosRS 52 7) $ S.map fromEnum $ uncards2S $ card7sS n
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
    "card7sS" -> logTick "card7sS" (uncards2S . card7sS) n
    -- 1.001
    "card7sSI" -> logTick "card7sSI ishuffle" (uncards2S . card7sSI) n
    _ -> pure ()

handRankComponents :: Int -> Text -> IO ()
handRankComponents n run = case run of
    -- 4.297
    "handRankAll" -> logTick "handRank" (fmap handRank) (card7s n)

    -- 1.964
    -- straight: 0.977
    -- flush: 0.623
    -- kind: 0.707
    --  rankCountS: 0.303
    "handRankS" -> logTick "handRankS"
      (applyS (fromEnum . handRankS))
      (Cards2S $ applyM (sortS . uncardsS) (card7sS n))

    -- 0.284
    "rank" -> do
      let !cs = uncards2S $ card7sS n
      logTick "rank for a flat"
        (applyFlat 7 (S.map (fromEnum . rank . toEnum . toIntegral))) cs

    -- 1.532
    "royals" -> do
      let !cs = uncards2S $ card7sS n
      logTick "flush n straight check"
         (applyFlat 7
          (fmap (fromEnum) . (\x -> flush x <|> straight x) . fmap (toEnum . toIntegral) . S.toList))
         cs

    -- 0.612
    "flush" -> do
      let !cs = uncards2S $ card7sS n
      logTick "flush"
         (applyFlat 7
          (fmap fromEnum . flush . fmap (toEnum . toIntegral) . S.toList))
         cs

    -- 0.623
    "flushS" -> do
      let !cs = uncards2S $ card7sS n
      logTick "flushS"
         (applyFlat 7 (flushS . CardsS))
         cs

    -- 1.082
    "straight" -> do
      let !cs = uncards2S $ card7sS n
      logTick "straight"
         (applyFlat 7
          (straight . fmap (toEnum . toIntegral) . S.toList))
         cs

    -- 0.977
    "straightS" -> do
      let !cs = uncards2S $ card7sS n
      logTick "straightS"
         (applyFlat 7 (straightS . RanksS))
         cs

    -- 1.507
    "kind" -> do
      let !cs = uncards2S $ card7sS n
      logTick "kind"
         (applyFlat 7 (kind . fmap rank . fmap (toEnum . toIntegral) . S.toList))
         cs

    -- 0.707
    "kindS" -> do
      let !cs = uncards2S $ card7sS n
      logTick "kindS"
         (applyFlat 7 (kindS . RanksS)) (S.map (`div` 4) cs)

    -- 0.707
    "oRankCount" -> do
      let !cs = uncards2S $ card7sS n
      logTick "oRankCount"
         (applyFlat 7 (oRankCount . RanksS))
         cs

    -- 1.887
    "rankCount" -> do
      let !cs = uncards2S $ card7sS n
      logTick "rankCount"
         (applyFlat 7 (rankCount . fmap (rank . toEnum . toIntegral) . S.toList))
         cs

    -- 0.307
    "rankCountS" -> do
      let !cs = uncards2S $ card7sS n
      logTick "rankCountS"
         (applyFlat 7 (rankCountS . RanksS))
         cs

    -- 1.432
    "suitRanks" -> do
      let !cs = uncards2S $ card7sS n
      logTick "suitRanks"
         (applyFlat 7 (suitRanks . fmap (toEnum . toIntegral) . S.toList))
         cs

    -- 0.039
    "handRankSum" -> do
      let !cs = uncards2S $ card7sS n
      logTick "sum"
         (applyFlatS 7 S.sum)
         cs

    -- 0.470
    "handRankToList" -> do
      let !cs = uncards2S $ card7sS n
      logTick "toList"
         (applyFlat 7 S.toList)
         cs

    -- 0.206
    "wrapping" -> do
      let !cs = uncards2S $ card7sS n
      logTick "wrapping"
         (applyFlat 7
          (sum . S.toList))
         cs

    _ -> pure ()

resolution :: Int -> Int -> Text -> IO ()
resolution p n run = case run of
    -- 4.183
    "bestLiveHand" ->
      logTick "bestLiveHand"  (fmap bestLiveHand) (tables p n)
    -- 3.967
    "tables" ->
      logTick "tables" (tables p) n
    -- 3.941
    "tablesB" ->
      logTick "tablesB" (tablesB p (Paired Ace) 0) n
    -- 3.964
    "winHand" ->
      logTick "winHand" (winHand (Paired Ace) 2) n
    -- 3.769
    "winOdds" ->
      logTick "winOdds" (sum . winOdds 2) (n `div` 169)
    _ -> pure ()

writeSome :: Int -> Text -> IO ()
writeSome n run = case run of
    "writeSomeStrats" ->
      logTickIO "writeSomeStrats" (writeSomeStrats n)
    _ -> pure ()

