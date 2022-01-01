{-# LANGUAGE FlexibleContexts #-}
module EclipticSurf.Chart where

import SwissEphemeris
import Almanac

import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Diagrams hiding (SVG)
import Control.Monad
import Data.Time
import Data.Foldable
import Diagrams.Backend.SVG (svgClass,  Options(SVGOptions), SVG(..), B )
import Diagrams hiding (render, aspect, Renderable)
import qualified Graphics.Svg as Svg
import Lucid (toHtmlRaw, Html)

render :: [Svg.Attribute] -> Double -> DEnv Double -> Renderable a -> Html ()
render attrs width' ev dia = 
  toHtmlRaw 
  . Svg.renderBS
  . renderDia SVG (SVGOptions (mkWidth width') Nothing "" attrs True) 
  $ toDiagram ev dia

renderEZ :: DEnv Double -> Renderable a -> Html ()
renderEZ = 
  render [Svg.makeAttribute "height" "not", Svg.makeAttribute "width" "not"] 600

surfChart :: String -> [Transit Planet] -> Renderable ()
surfChart title transits =
  toRenderable . execEC  $ do
    layout_title .= title
    layout_y_axis . laxis_reverse .= True
    forM_ transits $ \Transit{transitProgress, aspect, transiting, transited} ->
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
