module EclipticSurf.Views where

import Lucid
import EclipticSurf.Views.Layout (layout)

-- NOTE(luis) keeping this separate as I may want to introduce
-- a Reader context for rendering
render :: Html () -> Html ()
render = layout
