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
import EclipticSurf.Types (AppM)
import qualified EclipticSurf.Views.Home as Home
import qualified EclipticSurf.Views as Views
import qualified EclipticSurf.Chart as Chart
import EclipticSurf.Environment
import EclipticSurf.Query (currentTransits)
import Control.Lens
import qualified EclipticSurf.Views.Mundane as Mundane
import Data.Time
import SwissEphemeris (Planet(..))
import Data.Text hiding (null)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)


type Routes = ToServantApi Routes'
type Param' = QueryParam'  '[Required, Lenient]

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  , exploreMundane :: mode :- "explore-mundane" :> Get '[HTML] (Html ())
  , mundane ::
      mode :- "mundane"
      :> Param' "start" LocalTime
      :> Param' "end"   LocalTime
      :> QueryParams "transiting" Planet
      :> QueryParams "transited" Planet
      :> Get '[HTML] (Html ())
  } deriving stock (Generic)

server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { home = homeHandler
  , exploreMundane = mundaneForm
  , mundane = mundaneHandler
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
  -> m (Html ())
mundaneHandler start end transiting transited = do
  let errors = catMaybes [
                  either (const $ Just "Invalid start date") (const Nothing) start, 
                  either (const $ Just "Invalid end date") (const Nothing) end
                ]
  if not . null $ errors then
    renderView . Mundane.form . Just $ intercalate ", " errors
  else
    renderView $ Mundane.page start end transiting transited


renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Views.render

--- Parsing helpers

deriving stock instance Read Planet

instance FromHttpApiData Planet where
  parseUrlPiece t = do
    s <- parseUrlPiece t
    case (readMaybe s :: Maybe Planet) of
      Nothing -> Left . pack $ "Invalid planet"
      Just p -> Right p
