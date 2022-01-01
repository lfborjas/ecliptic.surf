{-# LANGUAGE FlexibleContexts #-}
module EclipticSurf.Chart where

import SwissEphemeris
import Almanac

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams hiding (SVG)
import Control.Monad
import Data.Time
import Data.Foldable
import Diagrams.Backend.SVG (svgClass,  Options(SVGOptions), SVG(..), B )
import Diagrams hiding (aspect, Renderable)
import qualified Graphics.Svg as Svg
import Lucid (toHtmlRaw, Html)

svgToHTML :: Svg.Element -> Html ()
svgToHTML =
  toHtmlRaw . Svg.renderBS 

renderSVG :: [Svg.Attribute] -> Double -> Diagram B -> Svg.Element
renderSVG attrs width' = renderDia SVG (SVGOptions (mkWidth width') Nothing "" attrs True)

toDiagram :: Renderable a -> IO (Diagram B)
toDiagram r = do
  e <- defaultEnv vectorAlignmentFns 800 600
  let (b, _) = runBackendR e r
  pure b

surfChart :: String -> [Transit Planet] -> Renderable ()
surfChart title transits =
  toRenderable . execEC  $ do
    layout_title .= title
    layout_y_axis . laxis_reverse .= True
    forM_ transits $ \Transit{transitProgress, aspect, transiting, transited} ->
      plot (fillBetween (show transiting <> " " <> show aspect <> " " <> show transited)
        [(dayFromJulianDay jd, (o, 5.0)) | (jd,o) <- toList transitProgress])

fillBetween :: String -> [(Day, (Double , Double))] -> EC l2 (PlotFillBetween Day Double)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle (0.4 `dissolve` color)
  plot_fillbetween_values .= vs
