module EclipticSurf.Views.Home where

-- TODO: check out:
-- https://github.com/flora-pm/flora-server/blob/development/src/FloraWeb/Templates/Types.hs

import Lucid

page :: Html () -> Html ()
page chart = 
  main_ $ do
    chart
