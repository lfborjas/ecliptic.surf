module EclipticSurf.Views.Home where

import Almanac (Transit (Transit, aspect, transited, transiting))
import Control.Monad (forM_)
import Data.List (intersperse)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601
import Lucid
import SwissEphemeris (Planet)

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
    
