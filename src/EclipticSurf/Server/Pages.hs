{-# LANGUAGE RecordWildCards #-}
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


type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ()) 
  } deriving stock (Generic)
  
server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { home = homeHandler
  }

homeHandler :: AppM sig m => m (Html ())
homeHandler = do
  Env{chartEnv} <- ask
  transits <- currentTransits
  let chart = Chart.surfChart (transits ^.. traversed . _1)
      rendered = Chart.renderEZ chartEnv chart 
  pure . Views.render . Home.page transits $ rendered
  
