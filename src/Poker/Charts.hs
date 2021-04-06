{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}

module Poker.Charts where

import NumHask.Prelude
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NumHask.Array.Fixed as A
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Chart hiding (shape)
import Lens.Micro
import Perf hiding (zero)
import qualified Data.Vector as V
import GHC.TypeLits
import qualified NumHask.Array.Shape as Shape
import qualified Control.Scanl as Scan
import Data.Mealy
import qualified Data.List as List
import qualified Prelude as P
import Poker.HandRank
import Poker.Types

-- | chart text in a Strat square format
--
-- >>> writeChartSvg "other/bs.svg" $ mempty & #chartList .~ [stratText]
--
-- ![bs example](other/bs.svg)
stratText :: Chart Double
stratText =
  Chart (TextA
           ( defaultTextStyle &
             #size .~ 0.03
           )
           (short <$> ((toEnum <$> [0..168]) :: [B])))
    (PointXY . (\(Point x y) -> Point (-x) y) <$> gs)
  where
    gs = grid MidPos (one :: Rect Double) (Point 13 13) :: [Point Double]

-- | pixel chart of an array of Bs
--
--
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  Strat Double ->
  [Chart Double]
bPixelChart pixelStyle plo s =
  runHud (aspect 1) hs1 cs1
  where
    pts = Point 13 13
    gr :: Rect Double
    gr = Rect 0 13 0 13
    f :: Point Double -> Double
    f (Point x y) = index s (toEnum $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo

bChart :: [Colour] -> Strat Double -> ChartSvg
bChart cs xs = mempty & #chartList .~
  stratText:
  bPixelChart
     (defaultSurfaceStyle & #surfaceColors .~ cs)
     (defaultSurfaceLegendOptions (pack "") &
      #sloStyle . #surfaceColors .~ cs)
     xs

-- | count of Bs
--
-- >>> writeChartSvg "other/count.svg" countChart
--
-- ![count example](other/count.svg)
countChart :: ChartSvg
countChart =
  bChart
  [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]
  (fromIntegral <$> countBs)

-- | map to a location
b2Chart :: (B -> Point Double) -> ChartSvg
b2Chart f = mempty & #hudOptions .~ (defaultHudOptions & #hudAxes %~ drop 1 & #hudCanvas .~ Nothing) & #chartList .~ [c]
  where
    c = Chart (TextA (defaultTextStyle & #size .~ 0.05 & #color %~ setOpac 0.1) ls) (fmap PointXY ps)
    ls = short <$> (toEnum <$> [0..168] :: [B])
    ps = f . toEnum <$> [0..168]

