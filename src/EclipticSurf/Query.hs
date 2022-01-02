{-# LANGUAGE OverloadedLists #-}

module EclipticSurf.Query where

import Data.Time
import Control.Lens
import Almanac
import Almanac.Optics
import Almanac.Extras
import Data.List.NonEmpty (fromList)
import SwissEphemeris (Planet (Sun), JulianDayTT)
import EclipticSurf.Types (AppM)
import EclipticSurf.Import


currentTransits :: AppM sig m => m [(Transit Planet, UTCTime, [UTCTime])]
currentTransits = do
  rn@(UTCTime today _) <- now
  todayTT' <- toJulianTT rn
  case todayTT' of
    Nothing -> error "Invalid date"
    Just todayTT -> do
      let (y,m,_) = toGregorian today
          monthStart = fromGregorian y m 1
          monthEnd = addGregorianMonthsClip 1 monthStart
          monthStartUT = UTCTime monthStart 0
          monthEndUT = UTCTime monthEnd 0
          q = mundane
                (Interval monthStartUT monthEndUT)
                [QueryPlanetaryMundaneTransit $ TransitOptions True (fromList relaxedAspects) (fromList relevantPairs)]
      exactEvents <- runExactQuery q
      let active = (summarize <$> exactEvents) ^.. traversed . _Just
          activeToday = 
            active
            & filter (happening todayTT . view _1)
          summarize evt =
            let transit = evt ^? eventL._PlanetaryTransitInfo
                allExact = evt ^?  exactitudeMomentsL  
                firstExact = evt ^? exactitudeMomentsL._head
            in (,,) <$> transit <*> firstExact <*> allExact
      pure activeToday

happening :: JulianDayTT -> Transit Planet -> Bool
happening today Transit{transitStarts, transitEnds} =
  transitStarts <= today && transitEnds >= today 

relevantPairs :: [(Planet, Planet)]
relevantPairs =
  defaultMundaneTransitPairs
  & filter ((/=) Sun . fst)

relaxedAspects :: [Aspect]
relaxedAspects = 
  majorAspects 
  & map (\a -> a{orbApplying = 5, orbSeparating  = 5})
