{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module EclipticSurf.Server where

import Colourista (successMessage)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import Data.Function ((&))
import qualified Data.Text as T
import EclipticSurf.Effects.Almanac (runAlmanacDataIO)
import EclipticSurf.Effects.Time (runTimeIO)
import EclipticSurf.Environment
  ( Config (deployEnv, ephePath, httpPort, precalcPath),
    Env (..),
    getServerEnv, DeployEnv (Development)
  )
import qualified EclipticSurf.Server.Pages as Pages
import EclipticSurf.Types (AppM)
import Graphics.Rendering.Chart.Backend (vectorAlignmentFns)
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Raw,
    ServerError,
    serveDirectoryWebApp,
    throwError,
    type (:>),
  )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT, genericServeT)
import SwissEphemeris (setEphemeridesPath)
import SwissEphemeris.Precalculated (setEphe4Path)
import System.Directory (makeAbsolute)
import Control.Monad (when)


data Routes route = Routes
  { assets :: route :- "static" :> Raw
  , pages  :: route :- Pages.Routes
  }
  deriving stock (Generic)

run :: IO ()
run = do
  config <- getServerEnv
  when (deployEnv config == Development) $
    successMessage . T.pack $ mconcat [
        "♊ [",
        show . deployEnv $ config,
        "] ",
        "Serving on port ",
        show . httpPort $ config,
        " ♊"
      ]
  denv <- defaultEnv vectorAlignmentFns 800 600
  absoluteEphePath <- makeAbsolute . ephePath $ config
  absoluteEp4Path  <- makeAbsolute . precalcPath $ config
  -- NOTE(luis) these shouldn't be necessary... and yet
  -- they are??
  setEphemeridesPath absoluteEphePath
  setEphe4Path absoluteEp4Path

  let environ = Env{chartEnv = denv, serverPort = httpPort config}
  runServer environ

runServer :: Env -> IO ()
runServer config = withStdoutLogger $ \logger -> do
  -- see: https://docs.servant.dev/en/stable/cookbook/generic/Generic.html
  let server = genericServeT (naturalTransform config) eclipticSurfServer
      warpSettings =
        defaultSettings
        & setPort (fromIntegral $ serverPort config)
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
            & runReader cfg
            & runTimeIO
            & runAlmanacDataIO
            & runM


eclipticSurfServer :: AppM sig m => Routes (AsServerT m)
eclipticSurfServer = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = Pages.server
  }
