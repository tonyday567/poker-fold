{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V
import Perf
import Poker.Card.Storable
import Poker.HandRank.Storable
import Poker.Random
import Prelude
import Data.Semigroup hiding (option)
import Options.Applicative
import System.Random
import GHC.Word
import qualified Data.Vector.Storable as S
import Control.DeepSeq
import Data.Bifunctor

data TestType = TestHandRankSParts | TestShuffle | TestDefault deriving (Eq, Show)

parseTestType :: Parser TestType
parseTestType =
  flag' TestHandRankSParts (long "handrankS" <> help "test handRankS speed") <|>
  flag' TestShuffle (long "shuffle" <> help "test shuffling") <|>
  pure TestDefault

data MeasureType' = MeasureTime' | MeasureSpace' | MeasureSpaceTime' | MeasureAllocation' | MeasureCount' deriving (Eq, Show)

parseMeasure' :: Parser MeasureType'
parseMeasure' =
  flag' MeasureTime' (long "time" <> help "measure time performance") <|>
  flag' MeasureSpace' (long "space" <> help "measure space performance") <|>
  flag' MeasureSpaceTime' (long "spacetime" <> help "measure both space and time performance") <|>
  flag' MeasureAllocation' (long "allocation" <> help "measure bytes allocated") <|>
  flag' MeasureCount' (long "count" <> help "measure count") <|>
  pure MeasureTime'

-- | unification of the different measurements to being a list of doubles.
measureLabels' :: MeasureType' -> [Text]
measureLabels' mt =
  case mt of
    MeasureTime' -> ["time"]
    MeasureSpace' -> spaceLabels
    MeasureSpaceTime' -> spaceLabels <> ["time"]
    MeasureAllocation' -> ["allocation"]
    MeasureCount' -> ["count"]

measureFinalStat :: MeasureType' -> [Double] -> Double
measureFinalStat mt =
  case mt of
    MeasureTime' -> average
    MeasureSpace' -> average
    MeasureSpaceTime' -> average
    MeasureAllocation' -> average
    MeasureCount' -> sum

data Options = Options
  { optionN :: Int,
    optionStatDType :: StatDType,
    optionTestType :: TestType,
    optionMeasureType :: MeasureType',
    optionExample :: Example,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (value 1000 <> long "runs" <> short 'n' <> help "number of tests to perform") <*>
  parseStatD <*>
  parseTestType <*>
  parseMeasure' <*>
  parseExample <*>
  parseGolden "golden" <*>
  parseReportConfig defaultReportConfig <*>
  switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "poker-fold benchmarking" <> header "speed performance")

count :: Measure IO (Sum Int)
count = toMeasure $ StepMeasure start stop
  where
    start = pure ()
    stop _ = pure 1

countN :: Int -> Measure IO Int
countN n = fmap sum $ toMeasureN n $ StepMeasure start stop
  where
    start = pure ()
    stop _ = pure 1

handRankS_ :: (MonadIO m, Semigroup t) => Cards -> PerfT m t HandRank
handRankS_ cs = do
  fl <- fap "flushS" flush cs
  case fl of
    Just x -> pure x
    Nothing -> do
      rs <- fap "ranksSet" ranksSet cs
      st <- fap "straightS" straight rs
      case st of
        Just x -> pure x
        Nothing -> do
          fap "kindS" kind (toRanks cs)

-- | unification of the different measurements to being averages.
measureD :: MeasureType' -> Measure IO [Double]
measureD mt =
  case mt of
    MeasureTime' -> (:[]) . fromIntegral <$> time
    MeasureSpace' -> toMeasure $ ssToList <$> space False
    MeasureSpaceTime' -> toMeasure ((\x y -> ssToList x <> [fromIntegral y]) <$> space False <*> stepTime)
    MeasureAllocation' -> (:[]) . fromIntegral <$> toMeasure (allocation False)
    MeasureCount' -> (:[]) . fromIntegral . sum <$> count

-- | unification of the different measurements to being averages.
measureDN :: MeasureType' -> Int -> Measure IO (Sum Double)
measureDN mt n = fmap (Sum . average) $
  case mt of
    MeasureTime' -> fmap fromIntegral <$> times n
    MeasureAllocation' -> fmap fromIntegral <$> toMeasureN n (allocation False)
    MeasureCount' -> (:[]) . fromIntegral <$> countN n
    x -> error (show x <> " NYI")

-- | unification of the different measurements to being averages.
measureD' :: MeasureType' -> Measure IO (Sum Double)
measureD' mt =
  case mt of
    MeasureTime' -> fmap (Sum . fromIntegral) time
    MeasureAllocation' -> fmap (Sum . fromIntegral) (toMeasure (allocation False))
    MeasureCount' -> fmap (fmap fromIntegral) count
    x -> error (show x <> " NYI")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let t = optionTestType o
  let mt = optionMeasureType o
  let gold' = optionGolden o
  let gold =
        case golden gold' of
          "other/golden.csv" ->
            gold'
            { golden = "other/" <>
              intercalate "-" [show t, show n, show mt] <>
              ".csv" }
          _ -> gold'
  let w = optionRawStats o
  let raw = "other/" <>
              intercalate "-" [show t, show n, show mt] <>
              ".map"
  let cfg = optionReportConfig o

  let eval = flip evalState (mkStdGen 69)

  let shuffleN = 7

  case t of
    TestShuffle -> do
      m <-
         execPerfT (measureDN mt n) $ do
          fap "rvi - single" (eval . rvi :: Word8 -> Word8) shuffleN
          fap "rviv - single" (eval . rviv 52 :: Word8 -> S.Vector Word8) shuffleN
          fap "rvis - single" (force . eval . rvis 52 :: Word8 -> [Word8]) shuffleN
          fap "rvi - single f" (force . eval . rvi :: Word8 -> Word8) shuffleN
          fap "rviv - single f" (force . eval . rviv 52 :: Word8 -> S.Vector Word8) shuffleN
          fap "cutShuffle" (force . eval . fmap (fst . cutShuffle 52 . S.toList) . rviv 52 :: Int -> V.Vector Int) (fromIntegral shuffleN)
          fap "indexShuffle" (force . eval . fmap (S.fromList . indexShuffle . S.toList) . rviv 52 :: Word8 -> S.Vector Word8) shuffleN
          fap "dealN" (eval . dealN :: Word8 -> Cards) shuffleN
          fap "card7sS" card7sS (fromIntegral shuffleN)
          fap "card7sSI" card7sSI (fromIntegral shuffleN)
      m' <-
         fmap (fmap (fmap (/fromIntegral n))) $
           execPerfT (measureD' mt) $ do
             fap "rvi - list" (eval . replicateM n . rvi :: Word8 -> [Word8]) shuffleN
             fap "rviv - list" (eval . replicateM n . rviv 52 :: Word8 -> [S.Vector Word8]) shuffleN
             fap "rvi - list f" (force . eval . replicateM n . rvi :: Word8 -> [Word8]) shuffleN
             fap "rviv - list f" (force . eval . replicateM n . rviv shuffleN :: Word8 -> [S.Vector Word8]) 52

      report cfg gold (measureLabels' mt) (Map.mapKeys (:[]) (fmap ((:[]) . getSum) (m <> m')))
    TestHandRankSParts -> do
      m <- fmap (fmap (measureFinalStat mt)) $
        execPerfT (measureD mt) $ V.sequence $ applyV handRankS_ (card7sS n)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels' mt) (Map.mapKeys (:[]) (fmap (:[]) m))
    TestDefault -> do
      m <- fmap (fmap (measureFinalStat mt)) $
        execPerfT (measureD mt) $ do
          _ <- fap "storable handRank max" (V.maximum . applyV handRank) (card7sS n)
          -- _ <- fap "list handRank max" (maximum . fmap L.handRank) (view (re L.cards7I) $ card7sS n)
          _ <- fap "storable handRank" (applyV handRank) (card7sS n)
          -- _ <- fap "handRank" (fmap L.handRank) (view (re L.cards7I) $ card7sS n)
          pure ()
      when w (writeFile raw (show m))
      report cfg gold (measureLabels' mt) (Map.mapKeys (:[]) (fmap (:[]) m))


rvis :: (RandomGen g, UniformRange e, Num e, Enum e) => e -> e -> State g [e]
rvis n k = sequence (rvi . (n -) <$> [0 .. (k - 1)])
