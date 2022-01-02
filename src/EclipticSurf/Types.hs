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
import Data.Time.Format.ISO8601 (iso8601ParseM, ISO8601)

type AppM sig m =
  ( 
    Has (Throw ServerError) sig m,
    Has (Reader Env) sig m,
    Has Time sig m,
    Has AlmanacData sig m
  )

-- | Newtype over TimeZone, to responsibly derive FromHttpApiData
newtype TimeZoneOffset 
  = TimeZoneOffset {getTimeZoneOffset :: TimeZone}
  deriving stock (Eq, Read, Show)
  deriving ISO8601 via TimeZone
  
instance FromHttpApiData TimeZoneOffset where
  parseUrlPiece t = do
    s <- parseUrlPiece t
    let tz = iso8601ParseM s
    case tz of
      Nothing -> Left . pack $ "Invalid timezone string " <> s
      Just parsed -> Right parsed


-- NOTE(luis) the orphans below only exist because these types are
-- from my own packages! Though maybe they should also be newtypes?

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
