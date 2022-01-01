{-# LANGUAGE ConstraintKinds #-}
module EclipticSurf.Types where

import EclipticSurf.Import
import Servant (ServerError)

type AppM sig m =
  ( Has (Throw ServerError) sig m)
