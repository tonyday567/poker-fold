{-# LANGUAGE OverloadedStrings #-}

-- | Range-based policies for multi-street poker.
module Poker.Policy
  ( InfoSet (..),
    Policy,
    ActionDist,
    policyProb,
    simplePolicy,
    thresholdPolicy,
  )
where

import Data.Bool (bool)
import Data.Functor.Rep (index, tabulate)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Poker.Action
import Poker.Range (Range)
import Poker.Table (Street (..))
import Prelude

-- | A minimal information set.
-- For the skeleton we keep it small: position, street, and whether the pot was already opened.
data InfoSet = InfoSet
  { infoPosition :: Int,
    infoStreet :: Street,
    infoOpened :: Bool
  }
  deriving (Eq, Show, Generic)

-- | An action distribution is a list of actions paired with a 'Range' of
-- probabilities, one probability per starting hand.
type ActionDist = [(Action, Range Double)]

-- | A policy maps an information set and a value range (e.g. equity) to an
-- action distribution.
type Policy = InfoSet -> Range Double -> ActionDist

-- | Probability that the policy assigns to an action for a given starting hand.
policyProb :: Policy -> InfoSet -> Range Double -> Action -> Range Double
policyProb pol is val action =
  fromMaybe (tabulate (const 0)) (List.lookup action (pol is val))

-- | A simple monotone policy.
-- When facing a bet: fold low, call middle, bet high.
-- When not facing a bet: check weak/middle, bet strong.
simplePolicy :: Double -> Double -> Policy
simplePolicy foldThresh callThresh (InfoSet _ _ opened) val =
  if opened
    then
      [ (Fold, tabulate (\sh -> bool 0 1 (index val sh < foldThresh))),
        (Call, tabulate (\sh -> bool 0 1 (index val sh >= foldThresh && index val sh < callThresh))),
        (Bet Pot, tabulate (\sh -> bool 0 1 (index val sh >= callThresh)))
      ]
    else
      [ (Check, tabulate (\sh -> bool 0 1 (index val sh < callThresh))),
        (Bet Pot, tabulate (\sh -> bool 0 1 (index val sh >= callThresh)))
      ]

-- | A polarized policy.
-- When facing a bet: fold weak, call middle, raise strong or as a bluff.
-- When not facing a bet: check weak/middle, bet strong or as a bluff.
thresholdPolicy ::
  Double ->
  -- ^ bet threshold (bet above this equity)
  Double ->
  -- ^ bluff threshold (bet below this equity)
  Double ->
  -- ^ fold threshold (fold below this)
  Policy
thresholdPolicy betThresh bluffThresh foldThresh (InfoSet _ _ opened) val =
  if opened
    then
      [ ( Fold,
          tabulate (\sh -> bool 0 1 (index val sh < foldThresh))
        ),
        ( Call,
          tabulate (\sh -> bool 0 1 (index val sh >= foldThresh && index val sh < betThresh))
        ),
        ( Bet Pot,
          tabulate (\sh -> bool 0 1 (index val sh >= betThresh || index val sh < bluffThresh))
        )
      ]
    else
      [ ( Check,
          tabulate (\sh -> bool 0 1 (index val sh >= bluffThresh && index val sh < betThresh))
        ),
        ( Bet Pot,
          tabulate (\sh -> bool 0 1 (index val sh >= betThresh || index val sh < bluffThresh))
        )
      ]
