{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}

-- | Chart examples.
module Poker.Charts
  ( sGrid,
    sRect,
    textChart,
    rectChart,
    pixelChart,
    scatterChart,
    colourText,
    colourBackground,
    colourGradient,
    writeAllCharts,
  )
where

import Chart hiding (shape)
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.Functor.Rep
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import GHC.Exts (fromList)
import Lens.Micro hiding (to)
import Poker hiding (fromList)
import Poker.Card.Storable
import Poker.RangedHand
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text
import Prelude

toText_ :: (Pretty a) => a -> Text
toText_ = renderStrict . layoutCompact . pretty

-- | A grid of points on the XY plane representing translation of the basis from B ~> 13x13 XY
sGrid :: RangedHand (Point Double)
sGrid = RangedHand $ fromList $ fmap (\(Point x y) -> Point (12 - y) x) (grid MidPos (Rect (-0.5) 0.5 (-0.5) 0.5) (Point 13 13) :: [Point Double])

-- | A grid of rectangles on the XY plane representing translation from B ~> 13x13 squares
sRect :: RangedHand (Rect Double)
sRect = (`addPoint` ((/ 13.0) <$> Rect (-0.5) 0.5 (-0.5) 0.5)) <$> sGrid

-- | text colour for Hand text charts.
colourText :: RangedHand Colour
colourText =
  tabulate $
    fromHandType (Colour 0 0 0.4 1, Colour 0 0.4 0 1, Colour 0.4 0 0 1)
      . to shapedHandS

-- | default background rectangle colour representing hand type.
colourBackground :: RangedHand Colour
colourBackground =
  tabulate $
    fromHandType
      (Colour 0.2 0.2 1 0.2, Colour 0.5 0.8 0.5 0.2, Colour 0.8 0.5 0.5 0.2)
      . to shapedHandS

-- | default colors represneting fold, call or raise.
fcrColours :: RawAction -> Colour
fcrColours = fromRawActionType (Colour 1 0 0 0.2, Colour 0 1 0 0.2, Colour 0 0 1 0.2)

fromHandType :: (a, a, a) -> ShapedHand -> a
fromHandType (a, _, _) (MkPair _) = a
fromHandType (_, a, _) (MkSuited _ _) = a
fromHandType (_, _, a) (MkOffsuit _ _) = a

-- | Rectangles in the RangedHand square with supplied fill color.
--
-- ![rect example](other/rect.svg)
rectChart :: RangedHand Colour -> ChartSvg
rectChart s =
  mempty & #chartList
    .~ toList
      ( liftR2
          (\r c -> Chart (RectA (RectStyle 0 c c)) [RectXY r])
          sRect
          s
      )

-- | chart text in a RangedHand square format with supplied text color.
--
-- ![text example](other/text.svg)
textChart :: RangedHand (Colour, Text) -> ChartSvg
textChart r =
  mempty & #chartList
    .~ zipWith
      ( \(c,t) p ->
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
      (toList r)
      (PointXY <$> toList sGrid)

-- | pixel chart of a RangedHand Double
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  RangedHand Double ->
  [Chart Double]
bPixelChart pixelStyle plo s =
  runHud (aspect 1) hs1 cs1
  where
    f :: Point Double -> Double
    f (Point x y) = index s (ShapedHandS $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle (Point 13 13) (Rect 0 13 0 13))
        plo

-- | Pixel chart of a RangedHand Double using supplied colour gradient.
pixelChartWith :: [Colour] -> RangedHand Double -> ChartSvg
pixelChartWith cs xs =
  mempty & #chartList
    .~ (textChart ((dark,) <$> stratText) ^. #chartList)
      <> bPixelChart
        (defaultSurfaceStyle & #surfaceColors .~ cs)
        ( defaultSurfaceLegendOptions (pack "")
            & #sloStyle . #surfaceColors .~ cs
        )
        xs

-- | Pixel chart of a RangedHand Double
--
-- ![pixel example](other/odds9.svg)
pixelChart :: RangedHand Double -> ChartSvg
pixelChart = pixelChartWith colourGradient

-- | default colour gradient for pixel charts
colourGradient :: [Colour]
colourGradient = [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]

-- | draw text Hands in the XY-plane
--
-- ![scatter example](other/compare29.svg)
scatterChart :: RangedHand (Point Double) -> ChartSvg
scatterChart ps = mempty & #hudOptions .~ (defaultHudOptions & #hudCanvas .~ Nothing) & #chartList .~ [c]
  where
    c = Chart (TextA (defaultTextStyle & #size .~ 0.04 & #color %~ setOpac 0.4) ls) (toList $ fmap PointXY ps)
    ls = toList $ array stratText

rectExample :: RangedHand Double -> ChartSvg
rectExample s =
  rectChart (fcrColours <$> rcf s 10 0.2 0.6)
    <> textChart ((,) <$> (setOpac 0.8 . fcrColours <$> rcf s 10 0.2 0.6) <*> stratText)
    & #hudOptions .~ (mempty & #hudAxes .~ [rankXAxis, rankYAxis])

rankXAxis :: AxisOptions
rankXAxis = defaultAxisOptions & #axisBar .~ Nothing & #place .~ PlaceTop & #axisTick . #tstyle .~ TickLabels (toText_ <$> reverse [Two .. Ace]) & #axisTick . #gtick .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.3))

rankYAxis :: AxisOptions
rankYAxis = defaultAxisOptions & #axisBar .~ Nothing & #place .~ PlaceLeft & #axisTick . #tstyle .~ TickLabels (toText_ <$> [Two .. Ace]) & #axisTick . #gtick .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.3))

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  (Just m) <- readSomeRanges
  let s = m Map.! "o2"
  writeChartSvg "other/text.svg" $
    textChart ((,) <$> colourText <*> stratText)
      <> rectChart colourBackground
  writeChartSvg "other/rect.svg" (rectExample s)
  writeChartSvg "other/count.svg" (pixelChart $ m Map.! "count")
  writeChartSvg "other/freq.svg" (pixelChart $ m Map.! "freq")
  writeChartSvg "other/odds2.svg" (pixelChart $ m Map.! "o2")
  writeChartSvg "other/odds9.svg" (pixelChart $ m Map.! "o9")
  writeChartSvg
    "other/top10.svg"
    (pixelChart (bool 0 1 <$> topBs (m Map.! "o2") 0.1))
  writeChartSvg
    "other/compare29.svg"
    (scatterChart (liftR2 (\x y -> Point x (8 * y)) (m Map.! "o2") (m Map.! "o9")))
