{-# OPTIONS_GHC -Wall #-}

import Options.Applicative
import Poker.Evaluate
import Poker.RangedHand
import Poker.Charts

import Prelude

data RunType = RunHvs7 | RunSomeRanges | RunCharts deriving (Eq, Show)

data Options = Options
  { optionSims :: Int,
    optionRunType :: RunType
  }
  deriving (Eq, Show)

sims :: Parser Int
sims =
  option auto (long "sims" <> short 's' <> help "number of simulations for RangedHand maps")
    <|> pure 10000

runType :: Parser RunType
runType =
  flag' RunHvs7 (long "hvs7" <> help "write hvs7.vec file") <|>
  flag' RunCharts (long "charts" <> help "write charts") <|>
  pure RunSomeRanges

options :: Parser Options
options =
  Options
    <$> sims
    <*> runType

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "poker-writes" <> header "Write poker-fold information to file")

main :: IO ()
main = do
  o <- execParser opts
  case optionRunType o of
    RunHvs7 -> do
      putStrLn "writing hvs7.vec"
      hvs7Write
    RunSomeRanges -> do
      putStrLn "writing some.str"
      writeSomeRanges (optionSims o)
    RunCharts -> do
      putStrLn "writing charts"
      writeAllCharts
