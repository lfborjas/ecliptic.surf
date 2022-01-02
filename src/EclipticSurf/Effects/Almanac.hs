
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module EclipticSurf.Effects.Almanac where

import Control.Algebra
import Almanac
    ( eventsWithExactitude, runQuery, ExactEvent, Query )
import Data.Kind ( Type )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Sequence (Seq)
import SwissEphemeris 
import Data.Time ( UTCTime )

data AlmanacData (m :: Type -> Type) k where
  RunExactQuery :: Query -> AlmanacData m (Seq ExactEvent)
  ToJulianTT :: UTCTime -> AlmanacData m (Maybe JulianDayTT)

runExactQuery :: Has AlmanacData sig m => Query -> m (Seq ExactEvent)
runExactQuery = send . RunExactQuery

toJulianTT ::  Has AlmanacData sig m => UTCTime -> m (Maybe JulianDayTT)
toJulianTT = send . ToJulianTT 

newtype AlmanacDataIOC m a = AlmanacDataIOC {runAlmanacDataIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

instance (MonadIO m, Algebra sig m) => Algebra (AlmanacData :+: sig) (AlmanacDataIOC m) where
  alg hdl sig ctx = case sig of
    L (RunExactQuery q) -> (<$ ctx) <$> liftIO (runQuery q >>= eventsWithExactitude)
    L (ToJulianTT t) -> (<$ ctx) <$> liftIO (toJulianDay t)
    R other -> AlmanacDataIOC (alg (runAlmanacDataIO . hdl) other ctx)
