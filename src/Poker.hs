{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
module Poker where

import NumHask.Prelude
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Short a where
  short :: a -> Text

  pretty :: (Foldable f) => f a -> IO ()
  pretty xs = putStrLn $ Text.intercalate "\n" $ short <$> toList xs

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Show, Enum)

instance Short Rank where
  short Two = "2"
  short Three = "3"
  short Four = "4"
  short Five = "5"
  short Six = "6"
  short Seven = "7"
  short Eight = "8"
  short Nine = "9"
  short Ten = "T"
  short Jack = "J"
  short Queen = "Q"
  short King = "K"
  short Ace = "A"

data Suit = Hearts | Clubs | Diamonds| Spades deriving (Eq, Show, Ord, Enum)

-- | see https://decodeunicode.org/en/u+1F0A2
instance Short Suit where
  short Hearts = "\9825"
  short Clubs = "\9827"
  short Diamonds = "\9826"
  short Spades = "\9824"

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)

instance Enum Card where
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
  toEnum x = let (d,m) = x `divMod` 4 in Card (toEnum d) (toEnum m)

instance Ord Card where
  (<=) c c' = rank c <= rank c'

instance Short Card where
  short (Card r s) = short r <> short s

instance (Functor f, Foldable f) => Short (f Card) where
  short cs = Text.intercalate "" (toList $ short <$> cs)

-- | a standard 52 card deck
--
-- >>> putStrLn $ short  $ Hand $ fromList $ sortOn (Down . rank) $ toList $ deck
-- A♠ A♣ A♦ A♠ K♠ K♣ K♦ K♠ Q♠ Q♣ Q♦ Q♠ J♠ J♣ J♦ J♠ T♠ T♣ T♦ T♠ 9♠ 9♣ 9♦ 9♠ 8♠ 8♣ 8♦ 8♠ 7♠ 7♣ 7♦ 7♠ 6♠ 6♣ 6♦ 6♠ 5♠ 5♣ 5♦ 5♠ 4♠ 4♣ 4♦ 4♠ 3♠ 3♣ 3♦ 3♠ 2♠ 2♣ 2♦ 2♠
deck :: Seq.Seq Card
deck = Seq.fromList $ Card <$> [Two .. Ace] <*> [Hearts .. Spades]

-- Seq combinators

-- | delete a value in a Seq and return the ripped ends.
cut1 :: Int -> Seq a -> (Seq a, Seq a)
cut1 x s = (Seq.take x s, Seq.drop (x+1) s)

-- | delete a value in a Seq and return the remaining Seq.
del1 :: Int -> Seq a -> Seq a
del1 x s = Seq.take x s <> Seq.drop (x+1) s

-- | Provide the element from a sequence at an index, together with the sequence with element removed.
get1 :: Seq.Seq a -> Int -> (a, Seq.Seq a)
get1 s x = (a, del1 x s)
  where
    (a Seq.:< _) = Seq.viewl (Seq.drop x s)

-- | Provide a random element from a sequence, togther with the sequence with element removed.
draw1 :: (RandomGen g) => g -> Seq.Seq a -> (g, a, Seq.Seq a)
draw1 g s = (g',e,s')
  where
    (e, s') = get1 s x
    n = length s
    (x, g') = uniformR (0, n-1) g

data Pack = Pack
  { dealt :: Seq.Seq Card,
    remPack :: Seq.Seq Card
  } deriving (Eq, Show, Generic)

pack0 :: Pack
pack0 = Pack Seq.Empty deck

data PackState = PackState
  { currentPack :: Pack,
    currentGen :: StdGen
  } deriving (Eq, Show, Generic)

defaultPackState :: Int -> PackState
defaultPackState x = PackState pack0 (mkStdGen x)

resetPack :: State PackState ()
resetPack = modify (\x -> x { currentPack = pack0 })

resetGen :: Int -> State PackState ()
resetGen r = modify (\x -> x { currentGen = mkStdGen r })

-- | draw a single card
-- >>> putStrLn $ foldMap id $ fmap short $ fst $ flip runState (PackState pack0 (mkStdGen 41)) (Seq.replicateM 2 draw)
-- A♠2♣
draw :: State PackState Card
draw = do
  (PackState p g) <- get
  let ((drawn, rem'), g') = first (get1 (remPack p)) (uniformR (0, length (remPack p) - 1) g)
  let p' = Pack (dealt p Seq.|> drawn) rem'
  put (PackState p' g')
  pure drawn

-- | draw n cards
--
-- >>> putStrLn $ short $ evalState (drawN 5) defaultPackState
-- A♠7♠T♠5♠6♣
--
-- putStrLn $ Text.intercalate "\n" $ short . hand <$> evalState (replicateM 10 (drawN 5)) defaultPackState
-- A♠T♠7♠6♣5♠
-- J♠9♠7♠7♠4♠
-- Q♣J♠J♠7♠3♣
-- K♠Q♣7♠5♠2♦
-- 9♣7♠3♣2♠2♠
-- K♠8♣8♣7♣3♦
-- K♣K♣T♠7♣4♠
-- A♠A♠K♦4♣3♦
-- A♠K♦T♣8♦4♣
-- A♠A♠T♣T♣7♣
drawN :: Int -> State PackState (Seq Card)
drawN n = Seq.replicateM n draw

newtype Hand = Hand { cards :: Seq.Seq Card } deriving (Eq, Show, Generic)

instance Short Hand where
  short (Hand cs) =
    Text.intercalate "" (toList $ fmap short cs)

hand :: Seq.Seq Card -> Hand
hand cs = Hand $ Seq.sortOn Down cs

ranks :: Hand -> Set.Set Rank
ranks (Hand cs) = Set.fromDescList $ toList $ rank <$> cs

suits :: Hand -> Set.Set Suit
suits (Hand cs) = Set.fromList $ toList $ suit <$> cs

data HandRank = HighCard (Seq.Seq Rank)
              | Pair  (Seq.Seq Rank)
              | TwoPair (Seq.Seq Rank)
              | ThreeOfAKind (Seq.Seq Rank)
              | Straight (Seq.Seq Rank)
              | Flush (Seq.Seq Rank)
              | FullHouse (Seq.Seq Rank)
              | FourOfAKind (Seq.Seq Rank)
              | StraightFlush (Seq.Seq Rank)
              deriving (Eq, Ord, Show)

instance Short HandRank where
  short (HighCard rs) = " 1:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Pair rs) = " 2:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (TwoPair rs) = "22:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (ThreeOfAKind rs) = " 3:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (FourOfAKind rs) = " 4:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (FullHouse rs) = "32:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Straight rs) = " S:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (Flush rs) = " F:" <>
    Text.intercalate "" (toList $ fmap short rs)
  short (StraightFlush rs) = "SF:" <>
    Text.intercalate "" (toList $ fmap short rs)

-- | compute the hand rank
--
-- >>> pretty $ handRank <$> hs
--  1:AT765
--  2:7J94
--  2:JQ73
--  1:KQ752
--  2:2973
--  2:8K73
--  2:KT74
--  2:AK43
--  1:AKT84
-- 22:AT7
handRank :: Hand -> HandRank
handRank h = fromMaybe (kind h)
  (sflush h <|>
   flush h <|>
   straight h)

-- | provide the ranks if the cards are an exact flush.
--
-- >>> flush (Seq.fromList [Card Ace Hearts, Card King Hearts, Card Queen Hearts])
-- Just (Flush (fromList [Queen,King,Ace]))
--
-- >>> flush (Seq.fromList [Card Ace Hearts, Card King Hearts, Card Queen Hearts, Card Jack Spades])
-- Nothing
flush :: Hand -> Maybe HandRank
flush h =
  bool
  Nothing
  (Just $ Flush $ rankFlush h)
  (isFlush h)

isFlush :: Hand -> Bool
isFlush h = 1 == Set.size (suits h)

rankFlush :: Hand -> Seq.Seq Rank
rankFlush h = fromList $ Set.toDescList $ ranks h

straight :: Hand -> Maybe HandRank
straight h =
  bool
  Nothing
  (Just $ Straight (Seq.singleton (rankStraight h)))
  (isStraight h)

isStraight :: Hand -> Bool
isStraight h =
  4 == (fromEnum (Set.findMax (suits h)) - fromEnum (Set.findMin (suits h)))

rankStraight :: Hand -> Rank
rankStraight h = Set.findMax (ranks h)

isSFlush :: Hand -> Bool
isSFlush h = isFlush h && isStraight h

sflush :: Hand -> Maybe HandRank
sflush h =
  bool
  Nothing
  (Just $ StraightFlush (Seq.singleton (rankStraight h)))
  (isSFlush h)

kind :: Hand -> HandRank
kind h = let rx@((r, x) Seq.:<| xs) = rankCount h in
  case x of
    4 -> FourOfAKind (Seq.singleton r)
    3 -> case xs of
          Seq.Empty -> ThreeOfAKind (Seq.singleton r)
          ((r', x') Seq.:<| _) ->
            bool
            (ThreeOfAKind (r Seq.:<| (fst <$> xs)))
            (FullHouse (r Seq.:<| r' Seq.:<| Seq.Empty)) (x'==2)
    2 -> case xs of
          Seq.Empty -> Pair (Seq.singleton r)
          ((r', x') Seq.:<| xs') ->
            bool
            (Pair (r Seq.:<| fmap fst xs))
            (TwoPair $ r Seq.:<| r' Seq.:<| fmap fst xs')
            (x' == 2)
    1 -> HighCard (fmap fst rx)
    _ -> HighCard (fmap fst rx)

rankCount :: Hand -> Seq.Seq (Rank, Int)
rankCount (Hand cs) =
  Seq.sortOn (\(r,x) -> Down (x,r)) $
  fromList $
  Map.toList $
  Map.fromDescListWith (+) $
  toList $
  fmap ((,1) . rank) cs

permute57 :: Seq a -> Seq (Seq a)
permute57 cs = Seq.fromList $
    (\x y -> Seq.take x cs <> Seq.take (y-x) (Seq.drop (x+1) cs) <> Seq.drop (5+x+y+2) cs)
    <$> [0..5]
    <*> [0..5]

rip75 :: Seq a -> [Seq a]
rip75 s = mconcat $ (`ripHole2` s) <$> [0 .. (Seq.length s - 1)]
  where
    ripHole2 x s = let (s0, s1) = cut1 x s in
      (s0 <>) . (\y -> uncurry (<>) $ cut1 y s1) <$> [0 .. (Seq.length s1 - 1)]

bestHand :: Seq.Seq Hand -> (Hand, Int)
bestHand hs = fromMaybe (Hand Seq.Empty, 0) $ head $ Seq.sortOn (Down . handRank . fst) (Seq.zip hs (Seq.iterateN (Seq.length hs) (+1) 0))

data TableState = TableState
  { players :: Seq.Seq (Seq.Seq Card),
    hole :: Seq.Seq Card
  } deriving (Eq, Show, Generic)

instance Short TableState where
  short (TableState ps h) =
    Text.intercalate ","
    (short <$> toList ps) <> ":" <>
    Text.intercalate " "
     [ short (Seq.take 3 h),
       short (Seq.take 1 $ Seq.drop 3 h),
       short (Seq.take 1 $ Seq.drop 4 h)
     ]

-- | deal a table from state
dealTable :: Int -> State PackState TableState
dealTable n = do
  resetPack
  ts
  where
    ps = Seq.replicateM n (drawN 2)
    h = drawN 5
    ts = TableState <$> ps <*> h

-- | Provide the best hand for each player
-- >>> ts0 = evalState (dealTable 1) defaultPackState
-- >>> resolve5 ts0
--
resolve5 :: TableState -> Seq Hand
resolve5 (TableState ps h) = fmap fst $
  (\p -> bestHand $ Seq.fromList $ hand <$> rip75 (p <> h)) <$> ps

-- | determine the winner for a table
--
-- >>> ts = evalState (replicateM 1000 (dealTable 9)) defaultPackState
-- >>> sum $ snd . winner <$> ts
-- 4052
winner :: TableState -> (Hand, Int)
winner ts = bestHand (resolve5 ts)
