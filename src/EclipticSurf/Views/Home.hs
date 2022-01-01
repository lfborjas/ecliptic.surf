module EclipticSurf.Views.Home where

-- TODO: check out:
-- https://github.com/flora-pm/flora-server/blob/development/src/FloraWeb/Templates/Types.hs

import Lucid

page :: Html ()
page = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      title_ "ecliptic.surf"
    body_ $ do
      main_ $ do
        "Hello"
