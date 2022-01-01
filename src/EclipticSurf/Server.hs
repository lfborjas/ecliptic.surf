{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module EclipticSurf.Server where

import EclipticSurf.Environment
import Colourista (successMessage)
import qualified Data.Text as T
import Network.Wai.Handler.Warp
import Data.Function
import Servant.Server.Generic
import Network.Wai.Logger
import Servant
import Servant.API.Generic
import EclipticSurf.Types
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import qualified EclipticSurf.Server.Pages as Pages

data Routes route = Routes
  { assets :: route :- "static" :> Raw
  , pages  :: route :- Pages.Routes
  }
  deriving stock (Generic)

run :: IO ()
run = do
  config <- getServerEnv
  successMessage . T.pack $ "♊ Serving on port" <> (show . httpPort $ config)
  runServer config

runServer :: Config -> IO ()
runServer config = withStdoutLogger $ \logger -> do
  -- see: https://docs.servant.dev/en/stable/cookbook/generic/Generic.html
  let server = genericServeT (naturalTransform config) eclipticSurfServer
      warpSettings =
        defaultSettings
        & setPort (fromIntegral $ httpPort config)
        & setLogger logger
  runSettings warpSettings server
  where
    naturalTransform cfg handler = do
      res <- runEffects
      either throwError pure res
      where
        runEffects =
          handler
            & runError @ServerError
            & runM


eclipticSurfServer :: AppM sig m => Routes (AsServerT m)
eclipticSurfServer = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = Pages.server
  }
