{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

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
import qualified Poker.HandRank.List as L
import Optics.Core

data TestType = TestHandRankSParts | TestDefault deriving (Eq, Show)

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

parseTestType :: Parser TestType
parseTestType =
  flag' TestHandRankSParts (long "handrankS" <> help "test handRankS speed") <|>
  pure TestDefault

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

  case t of
    TestHandRankSParts -> do
      m <- fmap (fmap (measureFinalStat mt)) $
        execPerfT (measureD mt) $ V.sequence $ applyV handRankS_ (card7sS n)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels' mt) (Map.mapKeys (:[]) (fmap (:[]) m))
    TestDefault -> do
      m <- fmap (fmap (measureFinalStat mt)) $
        execPerfT (measureD mt) $ do
          _ <- fap "storable handRank max" (V.maximum . applyV handRank) (card7sS n)
          _ <- fap "list handRank max" (maximum . fmap L.handRank) (view (re L.cards7I) $ card7sS n)
          _ <- fap "storable handRank" (applyV handRank) (card7sS n)
          _ <- fap "handRank" (fmap L.handRank) (view (re L.cards7I) $ card7sS n)
          pure ()
      when w (writeFile raw (show m))
      report cfg gold (measureLabels' mt) (Map.mapKeys (:[]) (fmap (:[]) m))
