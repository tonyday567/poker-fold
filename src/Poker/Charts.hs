{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
    rectChart,
    textChart,
    fcrExample,
    pixelChart,
    scatterChart,
    writeAllCharts,
  )
where

import Chart hiding (shape)
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Rep
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import GHC.Exts (fromList)
import Optics.Core hiding (to)
import Poker hiding (fromList)
import Poker.Card.Storable
import Poker.RangedHole
import Poker.Table
import Prettyprinter
import Prettyprinter.Render.Text
import Prelude

toText_ :: (Pretty a) => a -> Text
toText_ = renderStrict . layoutCompact . pretty

-- | A grid of points on the XY plane representing a 'RangedHole'
sGrid :: RangedHole (Point Double)
sGrid = RangedHole $ fromList $ fmap (\(Point x y) -> Point (- y) x) (grid MidPos (Rect (-0.5) 0.5 (-0.5) 0.5) (Point 13 13) :: [Point Double])

-- | A grid of rectangles on the XY plane representing a 'RangedHole'
sRect :: RangedHole (Rect Double)
sRect = (`addPoint` ((/ 13.0) <$> Rect (-0.5) 0.5 (-0.5) 0.5)) <$> sGrid

-- | text colour variation for Offsuit, Pair and Suited hole hands.
opsColourText :: RangedHole Colour
opsColourText =
  tabulate $
    fromOPS (Colour 0 0 0.4 1, Colour 0 0.4 0 1, Colour 0.4 0 0 1)
      . to shapedHoleS

-- | background rectangle style for Offsuit, Pair and Suited hole hands.
opsRectStyle :: (RectStyle, RectStyle, RectStyle)
opsRectStyle =
      (defaultRectStyle & #color .~ Colour 0.4 0.4 0.4 0.2 & #borderSize .~ 0,
       defaultRectStyle & #color .~ Colour 0.2 0.2 0.8 0.2 &
       #borderColor .~ dark & #borderSize .~ 0.001,
       defaultRectStyle & #color .~ Colour 0.8 0.2 0.2 0.2 & #borderSize .~ 0)

-- | default background representing Offsuit, Pair & Suited hole cards.
rhBackground :: RangedHole RectStyle
rhBackground = tabulate $ fromOPS opsRectStyle . to shapedHoleS

-- | default RangedHole Hud
rhHud :: ChartSvg
rhHud =
  mempty &
  #charts .~ unnamed [] &
  #hudOptions .~
    (mempty &
     #axes .~ [(5, rankXAxis), (5, rankYAxis)] &
     #titles .~
     [(10, defaultTitle "Suited" & #style % #size .~ 0.06 & #style % #color % opac' %~ 0.7),
      (10, defaultTitle "Offsuit" & #style % #size .~ 0.06 & #style % #color % opac' %~ 0.7 & #buffer .~ 0.08 & #place .~ PlaceLeft)
     ]
    )

-- | default X-Axis
rankXAxis :: AxisOptions
rankXAxis = defaultAxisOptions & #bar .~ Nothing & #place .~ PlaceTop & #ticks % #style .~ TickLabels (toText_ <$> reverse [Two .. Ace]) & #ticks % #gtick .~ Nothing & #ticks % #ltick .~ Nothing & #ticks % #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.4))

-- | default Y-Axis
rankYAxis :: AxisOptions
rankYAxis = defaultAxisOptions & #bar .~ Nothing & #place .~ PlaceLeft & #ticks % #style .~ TickLabels (toText_ <$> [Two .. Ace]) & #ticks % #gtick .~ Nothing & #ticks % #ltick .~ Nothing & #ticks % #ttick %~ fmap (first (\x -> x & #size .~ 0.04 & #color .~ Colour 0 0 0 0.3))

-- | default Offsuit-Pair-Suited legend.
opsLegend :: HudOptions
opsLegend =
  mempty &
  #legends .~ [(12, defaultLegendOptions & #content .~
                      let (o,p,s) = opsRectStyle in
                        [("Offsuit", RectChart o [one]),
                         ("Pair", RectChart p [one]),
                         ("Suited", RectChart s [one])]
               )]


-- | Rectangles in the RangedHole square with supplied fill color.
--
-- > writeChartSvg "other/rect.svg" $ rectChart rhBackground <> rhHud <> (mempty & #hudOptions .~ opsLegend) <> textChart ((,) <$> opsColourText <*> rhText)
--
-- ![rect example](other/rect.svg)
rectChart :: RangedHole RectStyle -> ChartSvg
rectChart s = mempty & #charts .~ unnamed (toList $ liftR2 (\r s -> RectChart s [r]) sRect s)

-- | Chart text with supplied text & colour.
--
-- The example below shows the winning chance headsup against any 2 cards.
--
-- > (Just m) <- readSomeRanges
-- > writeChartSvg "other/o2.svg" $ rectChart rhBackground <> rhHud <> textChart ((,) <$> opsColourText <*> (percent (Just 1) <$> m Map.! "o2"))
--
-- ![text example](other/o2.svg)
textChart :: RangedHole (Colour, Text) -> ChartSvg
textChart r =
  mempty & #charts
    .~ named "any2" (zipWith
      ( \(c,t) p ->
          TextChart
            (defaultTextStyle
                    & #size .~ 0.03
                    & #color .~ c
            )
            [(t,p)]
      )
      (toList r)
      (toList sGrid))

-- | The example chart below can be interpreted as raising with the top 20% of hands (blue), calling with the next 40% of hands (green) and folding the bottom 40% of hands (red).
--
-- ![fcr example](other/fcr.svg)
fcrExample :: RangedHole Double -> ChartSvg
fcrExample s =
  rectChart ((\b c -> b & #color .~ c) <$> rhBackground <*> (fcrBColour <$> rcf')) <>
  rhHud <>
  textChart ((\x -> (fcrTColour x, fcrText x)) <$> rcf')
  where
    rcf' = rcf s 10 0.2 0.6
    fcrBColour = fromRawActionType (Colour 1 0 0 0.2, Colour 0 1 0 0.2, Colour 0 0 1 0.2)
    fcrTColour = fromRawActionType (Colour 1 0 0 1, Colour 0.33 0.63 0.33 1, Colour 0 0 1 1)
    fcrText = fromRawActionType ("fold", "call", "raise")

-- | basic pixel chart of a RangedHole Double
bPixelChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  RangedHole Double ->
  ChartSvg
bPixelChart pixelStyle plo s = mempty & #charts .~ unnamed cs1 & #extraHuds .~ hs1
  where
    f :: Point Double -> Double
    f (Point x y) = index s (ShapedHoleS $ (12 - floor x) + 13 * floor y)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle (Point 13 13) (Rect 0 13 0 13))
        plo

-- | Pixel chart of a RangedHole Double using supplied colour gradient.
--
-- Odds of winning a showdown against 8 other hands
--
-- ![pixel example](other/odds9.svg)
pixelChart :: [Colour] -> RangedHole Double -> ChartSvg
pixelChart cs xs =
  bPixelChart
        (defaultSurfaceStyle & #surfaceColors .~ fromList cs)
        ( defaultSurfaceLegendOptions dark (pack "")
            & #sloStyle % #surfaceColors .~ fromList cs
        )
        xs

orderedScatterHud :: HudOptions
orderedScatterHud = defaultHudOptions & #axes .~ fmap (second (#ticks % #style .~ TickPlaced [(0, "worst"), (84.5, "median"), (168, "best")])) [ (5, defaultAxisOptions), (5, defaultAxisOptions & #place .~ PlaceLeft)] & #titles .~ [ (8, defaultTitle "Heads Up" & #place .~ PlaceTop & #style % #size .~ 0.08), (8, defaultTitle "Full Table" & #place .~ PlaceRight & #style % #size .~ 0.08)]

-- | draw text hole ranges in the XY-plane
--
-- The scatter chart example compares the ordering of hole cards given headsup versus a full table.
--
-- ![scatter example](other/compare29.svg)
scatterChart :: RangedHole (Point Double) -> ChartSvg
scatterChart ps = mempty & #hudOptions .~ (defaultHudOptions & #frames .~ []) & #charts .~ unnamed [c]
  where
    c = TextChart (defaultTextStyle & #size .~ 0.04 & #color % opac' %~ 0.4) (fromList $ toList $ (,) <$> rhText <*> ps)

-- | Make all the document charts.
writeAllCharts :: IO ()
writeAllCharts = do
  (Just m) <- readSomeRanges
  let s = m Map.! "o2"
  writeChartSvg "other/rect.svg" $
    rectChart rhBackground <>
    rhHud <>
    (mempty & #hudOptions .~ opsLegend) <>
    textChart ((,) <$> opsColourText <*> rhText)
  writeChartSvg "other/o2.svg" $ rectChart rhBackground <> rhHud <> textChart ((,) <$> opsColourText <*> (percent (Just 1) <$> m Map.! "o2"))
  writeChartSvg "other/fcr.svg" (fcrExample s)
  writeChartSvg
    "other/pixelo9.svg"
    (rhHud <> pixelChart [Colour 0.8 0.8 0.8 0.3, Colour 0 0.4 0.8 0.8, Colour 0 0.4 0.8 1] (m Map.! "o9"))
  writeChartSvg
    "other/compare29.svg"
    (scatterChart (liftR2 (\x y -> Point (fromIntegral x) (fromIntegral y)) (ordered $ m Map.! "o2") (ordered $ m Map.! "o9")) & #hudOptions .~ orderedScatterHud)
