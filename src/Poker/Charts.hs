{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Chart examples.
module Poker.Charts where

import Chart hiding (shape)
import Data.Functor.Rep
import qualified Data.Map.Strict as Map
import Lens.Micro
import NumHask.Prelude
import Poker

-- | chart text in a Strat square format.
--
-- ![text example](other/text.svg)
textChart :: (Basis -> Colour) -> Strat Text -> ChartSvg
textChart fcol s = mempty & #chartList .~
      zipWith
       (\(t,c) p ->
          Chart
          ( TextA
            ( defaultTextStyle
              & #size .~ 0.03
              & #color .~ c
            )
            [t]
          )
          [p]
       )
       (zip (toList s :: [Text]) (toList (fcol <$> (tabulate id :: Strat Basis))))
       ps
      where
        gs = grid MidPos (one :: Rect Double) (Point 13 13) :: [Point Double]
        ps = PointXY . (\(Point x y) -> Point (-x) y) <$> gs

toBType :: (a, a, a) -> Basis -> a
toBType (a,_,_) (Paired _) = a
toBType (_,a,_) (Suited _ _) = a
toBType (_,_,a) (Offsuited _ _) = a

-- | pixel chart of an array of Bs
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  Strat Double ->
  [Chart Double]
bPixelChart pixelStyle plo s =
  runHud (aspect 1) hs1 cs1
  where
    f :: Point Double -> Double
    f (Point x y) = index s (toEnum $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle (Point 13 13) (Rect 0 13 0 13))
        plo

-- | Pixel chart of a strategy in the Double domain.
bChart :: [Colour] -> Strat Double -> ChartSvg
bChart cs xs =
  mempty & #chartList
    .~ (textChart (const dark) stratText ^. #chartList) <>
  bPixelChart
    (defaultSurfaceStyle & #surfaceColors .~ cs)
    ( defaultSurfaceLegendOptions (pack "")
        & #sloStyle . #surfaceColors .~ cs
    )
    xs

-- | count of Bs
--
-- ![count example](other/count.svg)
countChart :: ChartSvg
countChart =
  bChart
    [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]
    (fromIntegral <$> enumBs)

-- | enumerate (Card, Card) and count the Bs
enumBs :: Strat Int
enumBs = tabulate (\k -> fromMaybe zero $ Map.lookup k (Map.fromListWith (+) ((,1) . fromPair <$> enum2 deck)))

-- | map to a location
b2Chart :: (Basis -> Point Double) -> ChartSvg
b2Chart f = mempty & #hudOptions .~ (defaultHudOptions & #hudAxes %~ drop 1 & #hudCanvas .~ Nothing) & #chartList .~ [c]
  where
    c = Chart (TextA (defaultTextStyle & #size .~ 0.05 & #color %~ setOpac 0.1) ls) (fmap PointXY ps)
    ls = short <$> (toEnum <$> [0 .. 168] :: [Basis])
    ps = f . toEnum <$> [0 .. 168]

-- | Given a Basis in position 0 headsup, what is the win rate against any two cards?
--
-- ![bwin example](other/bwin.svg)
bWinChart :: Int -> Int -> ChartSvg
bWinChart p n =
  bChart
    [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]
    (Strat (fromList ((\x -> winBasis x p n) <$> (toEnum <$> [0 .. 168]))))

-- | compare 2 player (x-axis) to 9 player (y-axis) win rates.
--
compare29 :: Basis -> Point Double
compare29 x = Point (winBasis x 1 1000) (winBasis x 8 1000)

-- | top x% Basis hands by 2 seat stats
--
-- ![compare example](other/compare29.svg)
compare29Chart :: ChartSvg
compare29Chart = b2Chart compare29

-- | top x% Basis hands by 2 seat stats
--
-- > (Just o2) <- readStrat "other/o2.str" :: IO (Maybe (Strat Double))
-- > writeChartSvg "other/best50.svg" (best2Chart o2 0.5)
--
-- ![best example](other/best50.svg)
best2Chart :: Strat Double -> Double -> ChartSvg
best2Chart bs x =
  bChart
  [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]
  (bool 0 1 <$> topBs bs x)

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  writeChartSvg "other/text.svg" (textChart (toBType (Colour 0.2 0.2 1 0.8, Colour 0.2 0.2 0.6 0.8, Colour 0.2 0.2 0.2 0.8)) stratText)
  writeChartSvg "other/count.svg" countChart
  writeChartSvg "other/compare29.svg" compare29Chart
  writeChartSvg "other/bwin.svg" (bWinChart 2 1000)
  (Just o2) <- readStrat "other/o2.str" :: IO (Maybe (Strat Double))
  writeChartSvg "other/best50.svg" (best2Chart o2 0.5)
