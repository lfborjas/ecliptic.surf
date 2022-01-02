{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module EclipticSurf.Types where

import EclipticSurf.Import
import Servant (ServerError, FromHttpApiData)
import EclipticSurf.Environment (Env)
import Data.Time
import SwissEphemeris (Planet(..))
import Servant.API (FromHttpApiData(parseUrlPiece))
import Almanac (AspectName(..))
import Text.Read (readMaybe)
import Data.Text (pack)

type AppM sig m =
  ( 
    Has (Throw ServerError) sig m,
    Has (Reader Env) sig m,
    Has Time sig m,
    Has AlmanacData sig m
  )

newtype TimeZoneOffset 
  = TimeZoneOffset {getTimeZoneOffset ::  Int}
  deriving stock (Eq, Read, Show)
  deriving FromHttpApiData via Int
  
offsetToTimeZone :: TimeZoneOffset -> TimeZone
offsetToTimeZone = hoursToTimeZone . getTimeZoneOffset

deriving stock instance Read Planet

instance FromHttpApiData Planet where
  parseUrlPiece t = do
    s <- parseUrlPiece t
    case (readMaybe s :: Maybe Planet) of
      Nothing -> Left . pack $ "Invalid planet"
      Just p -> Right p


deriving stock instance Read AspectName

instance FromHttpApiData AspectName where
  parseUrlPiece t = do
    s <- parseUrlPiece t
    case readMaybe s of
      Nothing -> Left . pack $ "Invalid aspect"
      Just a -> Right a
