{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StrictData #-}

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

-- | looking up HandRank given unordered cards
hrl :: S.Vector Word16 -> [Card] -> HandRank
hrl shr cs = (Map.!) fromLexi $ shr S.! (fromEnum (lexiIndex 52 7 $ (52 - ) . fromEnum <$> sortOn Down cs))

-- * random card generation

-- | random 7-Card vectors
--
card7s :: Int -> [[Card]]
card7s n = evalState (replicateM n (dealN 7)) (mkStdGen 42)

-- | random 7-Card vectors
--
card7sV :: Int -> V.Vector (S.Vector Int)
card7sV n = evalState (V.replicateM n (dealNV 7)) (mkStdGen 42)

-- * sorting

-- | isomorphic to shuffle, but keeps track of the sliced out bit.
--
-- > shuffle 52 (take 52 rvs52) == ishuffle rvs52
ishuffle' :: [Int] -> [Int]
ishuffle' as = Set.toDescList $ go as Set.empty
  where
    go [] s = s
    go (x0 : xs) s = go xs (Set.insert x1 s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + one) (d <= acc)) x0 s

dealN' :: (RandomGen g) => Int -> State g [Card]
dealN' n = fmap toEnum . ishuffle' <$> rvis 52 n

card7s' :: Int -> [[Card]]
card7s' n = evalState (replicateM n (dealN' 7)) (mkStdGen 42)

dealN'' :: (RandomGen g) => Int -> State g (V.Vector Card)
dealN'' n = V.map toEnum . V.take n . fst . shuffle 52 <$> rvis 52 n

card7s'' :: Int -> [V.Vector Card]
card7s'' n = evalState (replicateM n (dealN'' 7)) (mkStdGen 42)

logTick :: (NFData b) => Text -> (a -> b) -> a -> IO ()
logTick l x y = do
  (t, _) <- tick x y
  putStrLn (l <> ": " <> fixed (Just 3) (toSecs t))

toSecs :: Cycle -> Double
toSecs = ((/ 2.2e9) . P.fromIntegral)

main :: IO ()
main = do
  args' <- getArgs
  let (run, n) = case args' of
        [] -> ("poker", 1000000)
        (r':[]) -> (r', 1000000)
        (r':n':_) -> (r', fromMaybe 1000000 $ readMaybe n')
  case run of
    "rvi" -> logTick "rvi * 7" (\x -> evalState (replicateM (x * 7) (rvi 52)) (mkStdGen 42)) n
    "card7s" -> logTick "card7s" card7s n
    "card7sSet" -> logTick "card7s set" card7s' n
    "card7sShuffle" -> logTick "card7s shuffle" card7s'' n
    "card7sV" -> logTick "card7sV" card7sV n
    "sort" -> logTick "list sort" (fmap (sortOn Down)) (card7s n)
    "sortd" -> logTick "discrimination sort" (fmap (D.sortWith id)) (fmap fromEnum <$> card7s n)
    "sortseq" -> logTick "sequence sort" (fmap (Seq.sortOn Down)) (Seq.fromList <$> card7s n)
    "handRank" -> do
      (t, _) <- tick (fmap handRank) (card7s n)
      putStrLn ("handRank: " <> fixed (Just 3) (toSecs t))
    "poker-eval" -> do
      (t, _) <- tick (fmap (Deck.unNumericalHandValue . Poker.numericalHandValue_n 7)) (cs' n)
      putStrLn ("poker-eval: " <> fixed (Just 3) (toSecs t))
    "read" -> do
      (t, _) <- tickIO hvs7
      putStrLn ("instantiate hvs7: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    "write" -> do
      (t, _) <- tickIO (hvWrite n ("other/shr" <> show n <> ".vec"))
      putStrLn ("write handvalue vector: " <>
                show n <> " " <> fixed (Just 3) (toSecs t))
    "combinations52" -> do
      (t, _) <- tick (length . combinations n) [0..51::Int]
      putStrLn ("combinations n 52: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    "allhandranks" -> do
      (t, _) <- tickIO $ pure allHandRanks
      putStrLn ("allHands: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    "hrl" -> do
      s <- hvs7
      (t, _) <- tick (fmap (hrl s)) (card7s n)
      putStrLn ("lookup: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    "storable" -> do
      s <- hvs7
      (t, _) <- tick (fmap (s S.!)) (xs n)
      putStrLn ("Storable lookup: " <> show n <> " " <> fixed (Just 3) (toSecs t))
    _ -> P.error "not a thing"
  where
    xs n' = evalState (replicateM n' (rvi 100000)) (mkStdGen 42)
    cs' n' = fmap (Poker.fromList . fmap (\(Card x y) -> Poker.mkCard (toEnum . fromEnum $ x) (toEnum . fromEnum $ y))) (card7s n')
