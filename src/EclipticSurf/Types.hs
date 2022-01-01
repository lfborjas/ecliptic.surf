{-# LANGUAGE ConstraintKinds #-}
module EclipticSurf.Types where

import EclipticSurf.Import
import Servant (ServerError)
import EclipticSurf.Environment (Env)

type AppM sig m =
  ( 
    Has (Throw ServerError) sig m,
    Has (Reader Env) sig m,
    Has Time sig m,
    Has AlmanacData sig m
  )
