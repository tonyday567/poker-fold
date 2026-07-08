{-# OPTIONS_GHC -Wall #-}

import Options.Applicative
import Poker.Charts
import Poker.EquityMatrix (buildEquityMatrix, writeMatrix)
import Poker.EquitySuit (buildEquityMatrixSuit, writeSuitMatrix)
import Poker.HandRank (hvs7, hvs7Write)
import Poker.Range
import Prelude

data RunType = RunHvs7 | RunSomeRanges | RunCharts | RunEqMatrix Int | RunEqMatrixSuit Int deriving (Eq, Show)

data Options = Options
  { optionSims :: Int,
    optionRunType :: RunType
  }
  deriving (Eq, Show)

sims :: Parser Int
sims =
  option auto (long "sims" <> short 's' <> help "number of simulations for Range maps")
    <|> pure 10000

runType :: Parser RunType
runType =
  flag' RunHvs7 (long "hvs7" <> help "write hvs7.vec file")
    <|> flag' RunCharts (long "charts" <> help "write charts")
    <|> (RunEqMatrix <$> option auto (long "eqmatrix" <> short 'e' <> metavar "BOARDS" <> help "build equity matrix with N boards (e.g. 1000000)"))
    <|> (RunEqMatrixSuit <$> option auto (long "eqmatrix-suit" <> metavar "DEALS" <> help "build suit-aware equity matrix with N deals (e.g. 10000000)"))
    <|> pure RunSomeRanges

options :: Parser Options
options =
  Options
    <$> sims
    <*> runType

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "poker-fold-writes" <> header "Write poker-fold information to file")

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
    RunEqMatrix n -> do
      putStrLn $ "building equity matrix with " ++ show n ++ " boards... (this will take a while)"
      s <- hvs7
      let eq = buildEquityMatrix s n
      putStrLn "writing other/eqmatrix.str"
      writeMatrix eq "other/eqmatrix.str"
      putStrLn "done."
    RunEqMatrixSuit n -> do
      putStrLn $ "building suit-aware equity matrix with " ++ show n ++ " deals... (this will take a while)"
      s <- hvs7
      let eqSuit = buildEquityMatrixSuit s n
      putStrLn "writing other/eqmatrixSuit.str"
      writeSuitMatrix eqSuit "other/eqmatrixSuit.str"
      putStrLn "done."
