{-# LANGUAGE OverloadedLists #-}

module EclipticSurf.Query where

import Data.Time
import Control.Lens
import Almanac
import Almanac.Optics
import Almanac.Extras
import Data.List.NonEmpty (fromList)
import SwissEphemeris (Planet)


currentTransits :: IO [Transit Planet]
currentTransits = do
  (UTCTime today _) <- getCurrentTime
  let (y,m,_) = toGregorian today
      monthStart = fromGregorian y m 1
      monthEnd = addGregorianMonthsClip 1 monthStart
      monthStartUT = UTCTime monthStart 0
      monthEndUT = UTCTime monthEnd 0
      q = mundane
            (Interval monthStartUT monthEndUT)
            [QueryPlanetaryMundaneTransit $ easyTransitOptions (fromList majorAspects) (fromList defaultMundaneTransitPairs)]
  exactEvents <- runQuery q >>= eventsWithExactitude
  let active = (summarize <$> exactEvents) ^.. traversed . _Just
      summarize evt =
        let transit = evt ^? eventL._PlanetaryTransitInfo
            firstExact = evt ^? exactitudeMomentsL._head
        in (,) <$> transit <*> firstExact
  pure . map fst $ active
