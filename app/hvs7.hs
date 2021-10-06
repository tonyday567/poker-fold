{-# OPTIONS_GHC -Wall #-}

import Poker.Types
import Poker.Strategy
import Prelude
import Options.Applicative

data RunType = RunHvs7 | RunSomeStrats deriving (Eq, Show)

data Options = Options
  { optionSims :: Int,
    optionRunType :: RunType
  } deriving (Eq, Show)

sims :: Parser Int
sims =
  option auto (long "sims" <> short 's' <> help "number of simulations for Strat maps") <|>
  pure 10000

run :: Parser RunType
run =
  flag' RunHvs7 (long "hvs7" <> help "write hvs7.vec file") <|>
  pure RunSomeStrats

options :: Parser Options
options = Options <$>
  sims <*>
  run

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "poker-hvs7" <> header "Storable map writes for poker-fold")

main :: IO ()
main = do
  o <- execParser opts
  case optionRunType o of
    RunHvs7 -> do
      putStrLn "writing hvs7.vec"
      hvs7Write
    RunSomeStrats -> do
      putStrLn "writing some.str"
      writeSomeStrats (optionSims o)
