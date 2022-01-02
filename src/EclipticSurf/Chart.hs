{-# LANGUAGE FlexibleContexts #-}
module EclipticSurf.Chart where

import Almanac
    ( Transit(Transit, transitProgress, aspect, transiting,
              transited) )
import Control.Monad ( forM_ )
import Data.Foldable ( Foldable(toList) )
import Data.Time ( Day )
import Diagrams ( Diagram, renderDia, mkWidth )
import Diagrams.Backend.SVG (B, Options (SVGOptions), SVG (..))
import Graphics.Rendering.Chart.Backend.Diagrams
    ( DEnv, runBackendR )
import Graphics.Rendering.Chart.Easy
    ( Renderable,
      ToRenderable(toRenderable),
      EC,
      execEC,
      PlotFillBetween,
      solidFillStyle,
      laxis_reverse,
      layout_y_axis,
      plot_fillbetween_style,
      plot_fillbetween_title,
      plot_fillbetween_values,
      liftEC,
      plot,
      takeColor,
      dissolve,
      (.=) )
import qualified Graphics.Svg as Svg
import Lucid (Html, toHtmlRaw)
import SwissEphemeris ( Planet, dayFromJulianDay )

render :: [Svg.Attribute] -> Double -> DEnv Double -> Renderable a -> Html ()
render attrs width' ev dia = 
  toHtmlRaw 
  . Svg.renderBS
  . renderDia SVG (SVGOptions (mkWidth width') Nothing "" attrs True) 
  $ toDiagram ev dia

renderEZ :: DEnv Double -> Renderable a -> Html ()
renderEZ = 
  render [Svg.makeAttribute "height" "not", Svg.makeAttribute "width" "not"] 600

surfChart :: [Transit Planet] -> Renderable ()
surfChart transits =
  toRenderable . execEC  $ do
    layout_y_axis . laxis_reverse .= True
    forM_ (filter (not . null . transitProgress) transits) $ \Transit{transitProgress, aspect, transiting, transited} ->
      plot (fillBetween (show transiting <> " " <> show aspect <> " " <> show transited)
        [(dayFromJulianDay jd, (o, 5.0)) | (jd,o) <- toList transitProgress])

------------------------
-- HELPERS
------------------------

fillBetween :: String -> [(Day, (Double , Double))] -> EC l2 (PlotFillBetween Day Double)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle (0.4 `dissolve` color)
  plot_fillbetween_values .= vs

toDiagram :: DEnv Double -> Renderable a -> Diagram B
toDiagram e r =
  let (b, _) = runBackendR e r
  in b
