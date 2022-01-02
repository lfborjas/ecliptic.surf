{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module EclipticSurf.Server.Pages where

import EclipticSurf.Import
import Servant.API.Generic
import Lucid
import Servant.HTML.Lucid
import Servant
import Servant.Server.Generic
import EclipticSurf.Types 
import EclipticSurf.Environment
import EclipticSurf.Query (currentTransits, mundaneTransits, sansMoon)
import Control.Lens
import qualified EclipticSurf.Views.Mundane as Mundane
import Data.Time
import SwissEphemeris (Planet(..))
import Data.Text hiding (map, null)
import Data.Maybe (catMaybes)
import Data.Either (fromRight)
import Almanac (AspectName(..), Aspect (aspectName))
import Almanac.Extras (majorAspects)
import qualified EclipticSurf.Views.Home as Home
import qualified EclipticSurf.Views as Views
import qualified EclipticSurf.Chart as Chart


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
  renderView $ Mundane.form Nothing

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
    renderView . Mundane.form . Just $ intercalate ", " errors
  else do
    Env{chartEnv} <- ask
    today' <- now
    let today = utcToLocalTime utc today'
        startUT = localTimeToUTC utc (fromRight today start)
        endUT   = localTimeToUTC utc (fromRight today end)
        transiting' = if null transiting then sansMoon else transiting
        transited'  = if null transited then sansMoon else transited
        chosenAspects' = if null chosenAspects then map aspectName majorAspects else chosenAspects
    transits <- mundaneTransits startUT endUT transiting' transited' chosenAspects'
    let chart = Chart.surfChart $ transits ^.. traversed . _1
        rendered = Chart.renderEZ chartEnv chart
    renderView $ Mundane.page startUT endUT transiting' transited' chosenAspects' transits rendered

natalForm :: AppM sig m => m (Html ())
natalForm = undefined

natalHandler = undefined

renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Views.render
