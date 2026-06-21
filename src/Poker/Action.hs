{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Discrete action abstraction for multi-street poker.
module Poker.Action
  ( BetSize (..),
    Action (..),
    betSizeToDouble,
    toRawAction,
    legalActions,
    actionCost,
  )
where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Optics.Core ((^.))
import Poker.Table
import Prettyprinter
import Prelude

-- | Pot-relative bet sizes.
data BetSize = QuarterPot | HalfPot | Pot | TwoPot
  deriving (Eq, Show, Generic)

instance Pretty BetSize where
  pretty QuarterPot = "0.25p"
  pretty HalfPot = "0.5p"
  pretty Pot = "1p"
  pretty TwoPot = "2p"

-- | Player-facing actions.
data Action = Fold | Check | Call | Bet BetSize | AllIn
  deriving (Eq, Show, Generic)

instance Pretty Action where
  pretty Fold = "f"
  pretty Check = "x"
  pretty Call = "c"
  pretty (Bet s) = pretty s
  pretty AllIn = "ai"

-- | Multiplier used for each bet size relative to the pot-after-call.
betSizeToDouble :: BetSize -> Double
betSizeToDouble QuarterPot = 0.25
betSizeToDouble HalfPot = 0.5
betSizeToDouble Pot = 1.0
betSizeToDouble TwoPot = 2.0

-- | Compute the raw action and the amount added to the pot for the cursor seat.
-- Returns 'Nothing' if the action cannot be applied (e.g. folding when no bet).
toRawAction :: Table -> Action -> Maybe RawAction
toRawAction t action = case cursor t of
  Nothing -> Nothing
  Just p ->
    let maxBet = maximum (t ^. #bets)
        myBet = (t ^. #bets) !! p
        gap = maxBet - myBet
        myStack = (t ^. #stacks) !! p
        currentPot = t ^. #pot + sum (t ^. #bets)
        allInRaise = max 0 (myStack - gap)
     in case action of
          Fold -> bool (Just RawFold) Nothing (gap <= 0)
          Check -> bool Nothing (Just RawCall) (gap == 0)
          Call -> bool Nothing (Just RawCall) (gap > 0)
          AllIn ->
            bool
              (Just RawCall)
              (Just (RawRaise (gap + allInRaise)))
              (allInRaise > 0)
          Bet size ->
            let r = betSizeToDouble size * (currentPot + gap)
                r' = min (gap + r) myStack - gap
             in bool (Just RawCall) (Just (RawRaise r')) (r' > 0)

-- | Legal abstract actions for the cursor seat.
legalActions :: Table -> [Action]
legalActions t = case cursor t of
  Nothing -> []
  Just p ->
    let maxBet = maximum (t ^. #bets)
        gap = maxBet - ((t ^. #bets) !! p)
        sizes = [QuarterPot, HalfPot, Pot, TwoPot]
     in if gap <= 0
          then Check : (Bet <$> sizes) ++ [AllIn]
          else Fold : Call : (Bet <$> sizes) ++ [AllIn]

-- | Number of chips the cursor seat must add to the pot to take an action.
actionCost :: Table -> Action -> Double
actionCost t action = fromMaybe 0 $ do
  raw <- toRawAction t action
  p <- cursor t
  let maxBet = maximum (t ^. #bets)
      myBet = (t ^. #bets) !! p
      gap = maxBet - myBet
  pure $ case raw of
    RawFold -> 0
    RawCall -> gap
    RawRaise r -> gap + r
