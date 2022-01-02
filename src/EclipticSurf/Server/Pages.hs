module EclipticSurf.Server.Pages where

import Almanac (Aspect (aspectName), AspectName (..))
import Almanac.Extras (majorAspects)
import Control.Lens
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Text ( Text, intercalate )
import Data.Time
    ( ZonedTime(ZonedTime),
      localTimeToUTC,
      utcToLocalTime,
      utc,
      zonedTimeToUTC,
      LocalTime )
import qualified EclipticSurf.Chart as Chart
import EclipticSurf.Environment
import EclipticSurf.Import ( now, ask )
import EclipticSurf.Query (currentNatalTransits, currentTransits, mundaneTransits, natalTransits, sansMoon)
import EclipticSurf.Types ( AppM, TimeZoneOffset(..) )
import qualified EclipticSurf.Views as Views
import qualified EclipticSurf.Views.Home as Home
import qualified EclipticSurf.Views.SurfCharts as SurfCharts
import Lucid ( Html )
import Servant
    ( Lenient,
      Required,
      QueryFlag,
      QueryParam',
      QueryParams,
      type (:>),
      Get )
import Servant.API.Generic
    ( Generic, GenericMode(type (:-)), ToServant, ToServantApi )
import Servant.HTML.Lucid ( HTML )
import Servant.Server.Generic ( genericServerT, AsServerT )
import SwissEphemeris (Planet (..))


type Routes = ToServantApi Routes'
type Param' = QueryParam'  '[Required, Lenient]

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  , exploreMundane :: mode :- "explore-mundane" :> Get '[HTML] (Html ())
  , exploreNatal:: mode :- "explore-natal" :> Get '[HTML] (Html ())
  , mundane ::
      mode :- "mundane"
      :> Param' "start" LocalTime
      :> Param' "end"   LocalTime
      :> QueryParams "transiting" Planet
      :> QueryParams "transited" Planet
      :> QueryParams "aspects" AspectName
      :> Get '[HTML] (Html ())
  , natal ::
      mode :- "natal"
      :> Param' "dob"   LocalTime
      :> Param' "offset" TimeZoneOffset
      :> Param' "start" LocalTime
      :> Param' "end"   LocalTime
      :> QueryParams "transiting" Planet
      :> QueryParams "transited" Planet
      :> QueryParams "aspects" AspectName
      :> QueryFlag "includeActiveToday"
      :> Get '[HTML] (Html ())
  } deriving stock (Generic)

server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { home = homeHandler
  , exploreMundane = mundaneForm
  , mundane = mundaneHandler
  , exploreNatal = natalForm
  , natal = natalHandler
  }

homeHandler :: AppM sig m => m (Html ())
homeHandler = do
  Env{chartEnv} <- ask
  transits <- currentTransits
  let chart = Chart.surfChart (transits ^.. traversed . _1)
      rendered = Chart.renderEZ chartEnv chart
  renderView . Home.page transits $ rendered

mundaneForm :: AppM sig m => m (Html ())
mundaneForm = do
  renderView $ SurfCharts.mundaneForm Nothing

mundaneHandler
  :: AppM sig m
  => Either Text LocalTime
  -> Either Text LocalTime
  -> [Planet]
  -> [Planet]
  -> [AspectName]
  -> m (Html ())
mundaneHandler start end transiting transited chosenAspects= do
  let errors = catMaybes [
                  either (const $ Just "Invalid start date") (const Nothing) start,
                  either (const $ Just "Invalid end date") (const Nothing) end
                ]
  if not . null $ errors then
    renderView . SurfCharts.mundaneForm . Just $ intercalate ", " errors
  else do
    Env{chartEnv} <- ask
    today' <- now
    let today = utcToLocalTime utc today'
        startUT = localTimeToUTC utc (fromRight today start)
        endUT   = localTimeToUTC utc (fromRight today end)
        transiting' = defaultColl sansMoon transiting
        transited'  = defaultColl sansMoon transited
        chosenAspects' = defaultColl (map aspectName majorAspects) chosenAspects
    transits <- mundaneTransits startUT endUT transiting' transited' chosenAspects'
    let chart = Chart.surfChart $ transits ^.. traversed . _1
        rendered = Chart.renderEZ chartEnv chart
    renderView $ SurfCharts.mundanePage startUT endUT transiting' transited' chosenAspects' transits rendered

natalForm :: AppM sig m => m (Html ())
natalForm = renderView $ SurfCharts.natalForm Nothing

natalHandler
  :: AppM sig m
  => Either Text LocalTime
  -> Either Text TimeZoneOffset
  -> Either Text LocalTime
  -> Either Text LocalTime
  -> [Planet]
  -> [Planet]
  -> [AspectName]
  -> Bool
  -> m (Html ())
natalHandler dob tz start end transiting transited chosenAspects includeActiveToday = do
  let errors = catMaybes [
                  either (const $ Just "Invalid start date") (const Nothing) start,
                  either (const $ Just "Invalid end date") (const Nothing) end,
                  either (const $ Just "Invalid birth date") (const Nothing) dob,
                  either (const $ Just "Invalid timezone offset") (const Nothing) tz
                ]
  if not . null $ errors then
    renderView . SurfCharts.natalForm . Just $ intercalate ", " errors
  else do
    Env{chartEnv} <- ask
    today' <- now
    let today = utcToLocalTime utc today'
        startUT = localTimeToUTC utc (fromRight today start)
        endUT   = localTimeToUTC utc (fromRight today end)
        dobUT   =
          zonedTimeToUTC
            (ZonedTime (fromRight today dob)
            (getTimeZoneOffset (fromRight (TimeZoneOffset utc) tz)))
        transiting' = defaultColl sansMoon transiting
        transited'  = defaultColl sansMoon transited
        chosenAspects' = defaultColl (map aspectName majorAspects) chosenAspects
    transits <- natalTransits dobUT startUT endUT transiting' transited' chosenAspects'
    activeTransits <- if not includeActiveToday then pure mempty else currentNatalTransits dobUT
    let chart = Chart.surfChart $ transits ^.. traversed . _1
        rendered = Chart.renderEZ chartEnv chart
        renderedActive =
          if not . null $ activeTransits then
            Chart.renderEZ chartEnv . Chart.surfChart $ activeTransits ^.. traversed . _1
          else
            mempty
    renderView $ SurfCharts.natalPage dobUT startUT endUT transiting' transited' chosenAspects' transits rendered activeTransits renderedActive 

-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------

defaultColl :: [a] -> [a] -> [a]
defaultColl def val = if null val then def else val

renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Views.render
