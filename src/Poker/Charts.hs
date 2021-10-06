{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

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
import Data.Functor.Rep
import qualified Data.Map.Strict as Map
import Lens.Micro
import NumHask.Prelude
import Poker.Strategy
import Poker.Types
import GHC.OverloadedLabels
import Data.Text (Text, pack)
import Data.Bifunctor
import Prettyprinter
import Prettyprinter.Render.Text
import Poker hiding (fromList, Suited, Pair, Hand, Raise, Call, Fold, Seat)

-- >>> import Poker
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XTypeApplications
-- >>> import Lens.Micro
-- >>> import NumHask.Prelude
-- >>> import qualified Data.Text as Text
-- >>> (Just m) <- readSomeStrats
-- >>> s = m Map.! "o2"
--

toText_ :: (Pretty a) => a -> Text
toText_ = renderStrict . layoutCompact . pretty

-- | A grid of points on the XY plane representing translation of the basis from B ~> 13x13 XY
sGrid :: Strat (Point Double)
sGrid = Strat $ fromList $ fmap (\(Point x y) -> Point (- x) y) (grid MidPos (one :: Rect Double) (Point 13 13) :: [Point Double])

-- | A grid of rectangles on the XY plane representing translation from B ~> 13x13 squares
sRect :: Strat (Rect Double)
sRect = (`addPoint` ((/ 13.0) <$> one)) <$> sGrid

-- | text colour for Hand text charts.
colourText :: Strat Colour
colourText = tabulate $ fromHandType (Colour 0 0 0.4 1, Colour 0 0.4 0 1, Colour 0.4 0 0 1)

-- | default background rectangle colour representing hand type.
colourBackground :: Strat Colour
colourBackground = tabulate $ fromHandType (Colour 0.2 0.2 1 0.2, Colour 0.5 0.8 0.5 0.2, Colour 0.8 0.5 0.5 0.2)

-- | default colors represneting fold, call or raise.
fcrColours :: Action -> Colour
fcrColours = fromActionType (Colour 1 0 0 0.2, Colour 0 1 0 0.2, Colour 0 0 1 0.2)

fromHandType :: (a, a, a) -> Hand -> a
fromHandType (a, _, _) (Paired _) = a
fromHandType (_, a, _) (Suited _ _) = a
fromHandType (_, _, a) (Offsuited _ _) = a

-- | Rectangles in the Strat square with supplied fill color.
--
-- ![rect example](other/rect.svg)
rectChart :: Strat Colour -> ChartSvg
rectChart s =
  mempty & #chartList
    .~ toList
      ( liftR2
          (\r c -> Chart (RectA (RectStyle 0 c c)) [RectXY r])
          sRect
          s
      )

-- | chart text in a Strat square format with supplied text color.
--
-- ![text example](other/text.svg)
textChart :: Strat Colour -> Strat Text -> ChartSvg
textChart sc st =
  mempty & #chartList
    .~ zipWith
      ( \(t, c) p ->
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
      (zip (toList st :: [Text]) (toList sc))
      ps
  where
    gs = grid MidPos (one :: Rect Double) (Point 13 13) :: [Point Double]
    ps = PointXY . (\(Point x y) -> Point (- x) y) <$> gs

-- | pixel chart of a Strat Double
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

-- | Pixel chart of a Strat Double using supplied colour gradient.
pixelChartWith :: [Colour] -> Strat Double -> ChartSvg
pixelChartWith cs xs =
  mempty & #chartList
    .~ (textChart (tabulate $ const dark) stratText ^. #chartList)
      <> bPixelChart
        (defaultSurfaceStyle & #surfaceColors .~ cs)
        ( defaultSurfaceLegendOptions (pack "")
            & #sloStyle . #surfaceColors .~ cs
        )
        xs

-- | Pixel chart of a Strat Double
--
-- ![pixel example](other/odds9.svg)
pixelChart :: Strat Double -> ChartSvg
pixelChart = pixelChartWith colourGradient

-- | default colour gradient for pixel charts
colourGradient :: [Colour]
colourGradient = [Colour 0 1 0 0.3, Colour 0 0 1 0.3, Colour 1 0 0 0.3]

-- | draw text Hands in the XY-plane
--
-- ![scatter example](other/compare29.svg)
scatterChart :: Strat (Point Double) -> ChartSvg
scatterChart ps = mempty & #hudOptions .~ (defaultHudOptions & #hudCanvas .~ Nothing) & #chartList .~ [c]
  where
    c = Chart (TextA (defaultTextStyle & #size .~ 0.04 & #color %~ setOpac 0.4) ls) (toList $ fmap PointXY ps)
    ls = renderStrict . layoutCompact . pretty <$> (toEnum <$> [0 .. 168] :: [Hand])

rectExample :: Strat Double -> ChartSvg
rectExample s =
  rectChart (fcrColours <$> rcf s 10 0.2 0.6)
    <> textChart (setOpac 0.8 . fcrColours <$> rcf s 10 0.2 0.6) stratText
    & #hudOptions .~ (mempty & #hudAxes .~ [rankXAxis, rankYAxis])

rankXAxis :: AxisOptions
rankXAxis = defaultAxisOptions & #axisBar .~ Nothing & #place .~ PlaceTop & #axisTick . #tstyle .~ TickLabels (toText_ <$> reverse [Two .. Ace]) & #axisTick . #gtick .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.3))

rankYAxis :: AxisOptions
rankYAxis = defaultAxisOptions & #axisBar .~ Nothing & #place .~ PlaceLeft & #axisTick . #tstyle .~ TickLabels (toText_ <$> [Two .. Ace]) & #axisTick . #gtick .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.3))

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  (Just m) <- readSomeStrats
  let s = m Map.! "o2"
  writeChartSvg "other/text.svg" $
    textChart colourText stratText
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
