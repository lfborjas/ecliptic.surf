{-# LANGUAGE DeriveGeneric #-}
module EclipticSurf.Environment where

import Data.Word (Word16)
import GHC.Generics ( Generic )
import Env
import Text.Read (readMaybe)

data DeployEnv
  = Test
  | Development
  | Production
  deriving (Eq, Show, Read)

data Config = Config
  { httpPort :: !Word16
  , deployEnv :: !DeployEnv
  , ephePath :: !String
  , precalcPath :: !String
  } deriving stock (Show, Generic)

-- there could be another @Env@ data type
-- if we need to distinguish between the env variables
-- coming in and the "env" configuration used in the app

getServerEnv :: IO Config
getServerEnv = do
  parse id parseConfig

-- | Parse 'Config' from environment variables
parseConfig :: Parser Error Config
parseConfig =
  Config
  <$> parsePort
  <*> parseDeployEnv
  <*> parseEphePath
  <*> parsePrecalcPath


parsePort :: Parser Error Word16
parsePort =
  var port "PORT" (help "HTTP Port for server")

parseDeployEnv :: Parser Error DeployEnv
parseDeployEnv =
  var env "DEPLOY_ENV" (help "Environment to run the server as")

parseEphePath :: Parser Error String
parseEphePath =
  -- intentionally the same as required by SwissEphemeris
  var str "SE_EPHE_PATH" (help "Path to ephemeris data")


parsePrecalcPath :: Parser Error String
parsePrecalcPath =
  -- intentionally the same as required by SwissEphemeris
  var str "EP4_PATH" (help "Path to precalculated ephemeris data")
-------------------------------------------------------------------------------
--- PARSER HELPERS
-------------------------------------------------------------------------------

-- | Parse an arbitrary int
int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just parsed -> Right parsed

-- | Parse a valid http port number 
port :: Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort -> if intPort >= 1 && intPort <= 65535
    then Right $ fromIntegral intPort
    else Left . unread . show $ p

env :: Reader Error DeployEnv
env e = case readMaybe e of
  Nothing -> Left . unread . show $ e
  Just de -> Right de
