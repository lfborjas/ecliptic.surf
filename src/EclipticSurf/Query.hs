{-# LANGUAGE OverloadedLists #-}

module EclipticSurf.Query where

import Data.Time
import Control.Lens
import Almanac
import Almanac.Optics
import Almanac.Extras
import Data.List.NonEmpty (fromList)
import SwissEphemeris (Planet (Sun), JulianDayTT, GeographicPosition (GeographicPosition))
import EclipticSurf.Types (AppM)
import EclipticSurf.Import
import Data.Sequence (Seq)

mundaneTransits
  :: AppM sig m 
  => UTCTime
  -> UTCTime
  -> [Planet]
  -> [Planet]
  -> [AspectName]
  -> m [(Transit Planet, [UTCTime])]
mundaneTransits start end transiting transited aspects = do
  let opts = 
        TransitOptions 
          True 
          (fromList (relaxedAspects & filter ((`elem` aspects) . aspectName ))) 
          (fromList $ filteredPairs uniquePairs transiting transited)
      q = mundane
            (Interval start end)
            [QueryPlanetaryMundaneTransit opts]
  exactEvents <- runExactQuery q
  pure $ extractAllTransits exactEvents
     
natalTransits
  :: AppM sig m 
  => UTCTime
  -> UTCTime
  -> UTCTime
  -> [Planet]
  -> [Planet]
  -> [AspectName]
  -> m [(Transit Planet, [UTCTime])]
natalTransits dob start end transiting transited aspects = do
  let opts = 
        TransitOptions 
          True 
          (fromList (relaxedAspects & filter ((`elem` aspects) . aspectName ))) 
          (fromList $ filteredPairs uniquePairs transiting transited)
      q = natal 
            (Interval start end)
            (ReferenceEvent dob zeroGeo)
            [QueryPlanetaryNatalTransit opts]
  exactEvents <- runExactQuery q
  pure $ extractAllTransits exactEvents

-- | NOTE(luis) bogus geo position because we're not doing any house
-- calculations; consider making this optional in @almanac@?
zeroGeo :: GeographicPosition
zeroGeo = GeographicPosition 0 0
 
extractAllTransits :: Seq ExactEvent -> [(Transit Planet, [UTCTime])]
extractAllTransits exactEvents =
  (extract <$> exactEvents) ^.. traversed . _Just
  where
    extract evt =
      let info = evt ^? eventL._PlanetaryTransitInfo
          exacts = evt ^? exactitudeMomentsL
      in (,) <$> info <*> exacts

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
      pure activeToday

summarize :: ExactEvent -> Maybe (Transit Planet, UTCTime, [UTCTime])
summarize evt =
   let transit = evt ^? eventL._PlanetaryTransitInfo
       allExact = evt ^?  exactitudeMomentsL  
       firstExact = evt ^? exactitudeMomentsL._head
   in (,,) <$> transit <*> firstExact <*> allExact


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

sansMoon :: [Planet]
sansMoon = tail defaultPlanets
