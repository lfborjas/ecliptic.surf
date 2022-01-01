module EclipticSurf.Server.Pages where

import Servant.API.Generic
import Lucid
import Servant.HTML.Lucid
import Servant
import Servant.Server.Generic
import EclipticSurf.Types (AppM)
import qualified EclipticSurf.Views.Home as Home
import EclipticSurf.Views (render)

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ()) 
  } deriving stock (Generic)
  
server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { home = homeHandler
  }

homeHandler :: AppM sig m => m (Html ())
homeHandler = 
  pure $ render Home.page
  
