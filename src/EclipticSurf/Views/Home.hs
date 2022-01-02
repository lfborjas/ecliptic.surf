{-# LANGUAGE QuasiQuotes #-}

module EclipticSurf.Views.Home where

import Almanac (Transit (Transit, aspect, transited, transiting))
import Control.Monad (forM_)
import Data.List (intersperse)
import Data.Time (UTCTime)
import Lucid
import SwissEphemeris (Planet)
import EclipticSurf.Views.Helpers (dateTimeShow, markdownToHtml)
import PyF

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
              mconcat . intersperse "," $ map dateTimeShow exacts
            ]

about :: Html ()
about = markdownToHtml [fmt|
# About

Surf charts for planetary transits. I call 'em surf charts because they look like waves -- the crest
being when the transit is closest to exactitude.

The calculations are done via my [almanac](https://github.com/lfborjas/almanac), which
uses my [haskell bindings](https://github.com/lfborjas/swiss-ephemeris)
to the excellent [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm).

This website is [open source](https://github.com/lfborjas/ecliptic.surf), under the [AGPL-3](https://www.gnu.org/licenses/agpl-3.0.en.html) license. 
Made in a febrile weekend by [a sentient potato](https://www.lfborjas.com/)

## Misc. Notes

* You'll note moon transits are absent: the moon moves very fast most of the time, so to capture
  all aspects it can possibly make, one would have to either look at smaller increments, or use
  a different technique. Moon transits _are_ provided for "current natal transits".
* There's a lot of fanciness we could add here: navigate between days or expand the range,
  click on transits to see their graph, make the colors repeat less often, etc. I may add that stuff!
  But, unlike [freenatalchart.xyz](https://freenatalchart.xyz/), I'm mostly building this lil' tool just
  so I can plot transit activity for transits that show up on astro.com lol.
|]
