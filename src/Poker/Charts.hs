{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Chart examples.
module Poker.Charts
  ( sGrid,
    sRect,
    opsColourText,
    opsRectStyle,
    opsLegend,
    rankXAxis,
    rankYAxis,
    rhBackground,
    rhHud,
    baseChart,
    percentChart,
    rectChart,
    textChart,
    fcrExample,
    pixelChart,
    scatterChart,
    orderedScatterHud,
    writeAllCharts,
    pixelColors,
  )
where

import Chart hiding (shape, Range)
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Rep
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import GHC.Exts (fromList)
import Optics.Core hiding (to)
import Poker.Range
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text
import Prelude
import Poker.Card as C ( Rank(Ace, Two) )

toText_ :: (Pretty a) => a -> Text
toText_ = renderStrict . layoutCompact . pretty

-- | A grid of points on the XY plane representing a 'Range'
sGrid :: Range (Point Double)
sGrid = Range $ fromList $ fmap (\(Point x y) -> Point (-y) x) (grid MidPos (Rect (-0.5) 0.5 (-0.5) 0.5) (Point 13 13) :: [Point Double])

-- | A grid of rectangles on the XY plane representing a 'Range'
sRect :: Range (Rect Double)
sRect = (`addPoint` ((/ 13.0) <$> Rect (-0.5) 0.5 (-0.5) 0.5)) <$> sGrid

-- | text colour variation for Offsuit, Pair and Suited hole hands.
opsColourText :: Double -> Double -> Double -> (Colour, Colour, Colour)
opsColourText l c op =
  (view lcha2colour' (LCHA l c 15 op),
   view lcha2colour' (LCHA l c 126 op),
   view lcha2colour' (LCHA l c 260 op))

-- | background rectangle style for Offsuit, Pair and Suited hole hands.
opsRectStyle :: Double -> Double -> Double -> (RectStyle, RectStyle, RectStyle)
opsRectStyle l c op =
  ( defaultRectStyle & #color .~ o & #borderSize .~ 0,
    defaultRectStyle
      & #color .~ p
      & #borderColor .~ set opac' 0.7 (over lightness' 0.3 p)
      & #borderSize .~ 0.002,
    defaultRectStyle & #color .~ s & #borderSize .~ 0
  )
  where
    (o,p,s) = opsColourText l c op

-- | default background representing Offsuit, Pair & Suited hole cards.
rhBackground :: Range RectStyle
rhBackground = ops (opsRectStyle 0.3 0.2 0.2)

-- | default Chart.Range Hud
rhHud :: Double -> HudOptions
rhHud op =
      mempty
             & #axes .~ [(5, rankXAxis op), (5, rankYAxis op)]
             & #titles
               .~ [ (10, defaultTitle "Suited" & #style % #size .~ 0.06 & #style % #color % opac' .~ op),
                    (10, defaultTitle "Offsuited" & #style % #size .~ 0.06 & #style % #color % opac' .~ op & #buffer .~ 0.05 & #place .~ PlaceLeft)
                  ]

-- | default X-Axis
rankXAxis :: Double -> AxisOptions
rankXAxis op = defaultAxisOptions & #bar .~ Nothing & #place .~ PlaceTop & #ticks % #style .~ TickLabels (toText_ <$> reverse [Two .. Ace]) & #ticks % #gtick .~ Nothing & #ticks % #ltick .~ Nothing & #ticks % #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ set opac' op dark))

-- | default Y-Axis
rankYAxis :: Double -> AxisOptions
rankYAxis op = defaultAxisOptions & #bar .~ Nothing & #place .~ PlaceLeft & #ticks % #style .~ TickLabels (toText_ <$> [Two .. Ace]) & #ticks % #gtick .~ Nothing & #ticks % #ltick .~ Nothing & #ticks % #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ set opac' op dark))

-- | default Offsuit-Pair-Suited legend.
opsLegend :: Double -> HudOptions
opsLegend op =
  over #legends (fmap (second (over #frame (fmap (#color .~ transparent))))) $
  colourHudOptions (set opac' op) $
    mempty & #legends .~ [(30,
             defaultLegendOptions
               & #content
                 .~ let (o, p, s) = opsRectStyle 0.3 0.2 0.4
                     in [ ("Offsuited", [RectChart (o & set (#color % opac') op) [one]]),
                          ("Paired", [RectChart (p & set (#color % opac') op) [one]]),
                          ("Suited", [RectChart (s & set (#color % opac') op) [one]])
                        ])]

-- | Rectangles in the Chart.Range square with supplied fill color.
--
-- > writeChartOptions "other/rect.svg" $ rectChart rhBackground <> rhHud <> (mempty & #hudOptions .~ opsLegend) <> textChart ((,) <$> opsColourText <*> rhText)
--
-- ![rect example](other/rect.svg)
rectChart :: Range RectStyle -> ChartOptions
rectChart s = mempty & #charts .~ unnamed (toList $ liftR2 (\r s -> RectChart s [r]) sRect s)

-- | Chart text with supplied text & colour.
--
-- The example below shows the winning chance headsup against any 2 cards.
--
-- > (Just m) <- readSomeRanges
-- > writeChartOptions "other/o2.svg" $ rectChart rhBackground <> rhHud <> textChart ((,) <$> opsColourText <*> (percent (Just 1) <$> m Map.! "o2"))
--
-- ![text example](other/o2.svg)
textChart :: Range (Colour, Text) -> ChartOptions
textChart r =
  mempty
    & #charts
      .~ named
        "any2"
        ( zipWith
            ( \(c, t) p ->
                TextChart
                  ( defaultTextStyle
                      & #size .~ 0.03
                      & #color .~ c
                  )
                  [(t, p)]
            )
            (toList r)
            (toList sGrid)
        )

-- | The example chart below can be interpreted as raising with the top 20% of hands (blue), calling with the next 40% of hands (green) and folding the bottom 40% of hands (red).
--
-- ![fcr example](other/fcr.svg)
fcrExample :: Range Double -> ChartOptions
fcrExample s =
  rectChart ((\b c -> b & #color .~ c) <$> rhBackground <*> (fcrBColour <$> rcf'))
    <> textChart ((\x -> (fcrTColour x, fcrText x)) <$> rcf') &
    #hudOptions %~ (<> rhHud 0.7)
  where
    rcf' = rcf s 10 0.2 0.6
    fcrBColour = fromRawActionType (opsColourText 0.3 0.05 0.05)
    fcrTColour = fromRawActionType (opsColourText 0.5 0.15 1)
    fcrText = fromRawActionType ("fold", "call", "raise")

-- | basic pixel chart of a Range Double
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  Range Double ->
  ChartOptions
bPixelChart pixelStyle plo s = mempty & #charts .~ runHud (aspect 1) hs1 (addHud (rhHud 0.7) $ unnamed cs1)
  where
    f :: Point Double -> Double
    f (Point x y) = index s (StartingHandS $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle (Point 13 13) (Rect 0 13 0 13))
        plo

-- | Pixel chart of a Range Double using supplied colour gradient.
--
-- Odds of winning a showdown against 8 other hands
--
-- ![pixel example](other/odds9.svg)
pixelChart :: [Colour] -> Range Double -> ChartOptions
pixelChart cs xs =
  bPixelChart
    (defaultSurfaceStyle & #surfaceColors .~ fromList cs)
    ( defaultSurfaceLegendOptions dark (pack "drift")
        & #sloLegendOptions % #place .~ PlaceAbsolute (Point 0.6 (-0.2))
        & #sloStyle % #surfaceColors .~ fromList cs
    )
    xs

pixelColors :: [Colour]
pixelColors =
  [ view lcha2colour' (LCHA 0.3 0.1 20 1),
    view lcha2colour' (LCHA 0.5 0.2 350 1),
    view lcha2colour' (LCHA 0.6 0.3 330 1)
  ]

orderedScatterHud :: HudOptions
orderedScatterHud =
  defaultHudOptions &
  #axes .~ fmap (second (#ticks % #style .~ TickPlaced [(0, "worst"), (84.5, "median"), (168, "best")])) [(5, defaultAxisOptions), (5, defaultAxisOptions & #place .~ PlaceLeft)] &
  #titles .~ [(4, defaultTitle "Heads Up" & #place .~ PlaceTop & #style % #size .~ 0.08), (4, defaultTitle "Full Table" & #place .~ PlaceRight & #style % #size .~ 0.08)]

-- | draw text hole ranges in the XY-plane
--
-- The scatter chart example compares the ordering of hole cards given headsup versus a full table.
--
-- ![scatter example](other/compare29.svg)
scatterChart :: Range (Point Double) -> ChartOptions
scatterChart ps =
  mempty &
  #markupOptions % #markupHeight .~ 600 &
  #markupOptions % #cssOptions % #cssExtra .~ fillSwitch (dark, light) "dark" "scatter" &
  #hudOptions .~ (defaultHudOptions & #frames .~ []) &
  #charts .~ named "scatter" [c]
  where
    c = TextChart
      (defaultTextStyle & #size .~ 3 & #color % opac' %~ 0.6)
      (fromList $ toList $ (,) <$> rhText <*> ps)

baseChart :: ChartOptions
baseChart =
    rectChart rhBackground
      <> textChart ((,) <$> ops (opsColourText 0.6 0.2 1) <*> rhText)
      & #hudOptions %~ (<> rhHud 0.7)
      & #hudOptions %~ (<> opsLegend 0.7)
      & #markupOptions % #markupHeight .~ 600
      & #hudOptions % #chartAspect .~ CanvasAspect 1

percentChart :: Range Double -> ChartOptions
percentChart r = rectChart rhBackground <> textChart ((,) <$> ops (opsColourText 0.6 0.2 1) <*> (fixed (Just 0) . (100*) <$> r)) & #hudOptions %~ (<> rhHud 0.7)

drift :: Range Double -> Range Double -> Range Double
drift o2 o9 = (\o o' -> o' * 9 - o * 2) <$> o2 <*> o9

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  (Just m) <- readSomeRanges
  let o2 = m Map.! "o2"
  let o9 = m Map.! "o9"
  writeChartOptions "other/base.svg" baseChart
  writeChartOptions "other/o2.svg" $ percentChart o2
  writeChartOptions "other/o9.svg" $ percentChart o9
  writeChartOptions "other/fcr2.svg" (fcrExample o2)
  writeChartOptions "other/fcr9.svg" (fcrExample o9)
  writeChartOptions
    "other/pixelo9.svg"
    (pixelChart pixelColors (drift o2 o9))
  writeChartOptions
    "other/compare29.svg"
    (scatterChart (liftR2 (\x y -> Point (fromIntegral x) (fromIntegral y)) (ordered o2) (ordered o9)) & #hudOptions .~ orderedScatterHud)
