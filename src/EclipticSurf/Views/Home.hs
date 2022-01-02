module EclipticSurf.Views.Home where

-- TODO: check out:
-- https://github.com/flora-pm/flora-server/blob/development/src/FloraWeb/Templates/Types.hs

import Lucid
import Data.Time
import SwissEphemeris (Planet)
import Almanac
import Control.Monad
import Data.List (intersperse)
import Data.Time.Format.ISO8601

page :: [(Transit Planet, UTCTime, [UTCTime])] -> Html () -> Html ()
page transits  chart = 
  main_ $ do
    p_ [class_  "mt-2"] $ do 
      "Transits active today, and their activity this month."
    chart
    ul_ $ do
      forM_ transits $ \(Transit{transiting, transited, aspect}, _firstExact, exacts) -> do
        li_  $ do
          toHtml . mconcat . intersperse " " $ [
              show transiting,
              show aspect,
              show transited,
              "exact at:",
              mconcat . intersperse "," $ map iso8601Show exacts
            ]
    
