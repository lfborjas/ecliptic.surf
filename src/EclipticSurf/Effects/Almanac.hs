
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
import Data.Kind
import Control.Monad.IO.Class
import Data.Sequence (Seq)

data AlmanacData (m :: Type -> Type) k where
  RunExactQuery :: Query -> AlmanacData m (Seq ExactEvent)

runExactQuery :: Has AlmanacData sig m => Query -> m (Seq ExactEvent)
runExactQuery = send . RunExactQuery


newtype AlmanacDataIOC m a = AlmanacDataIOC {runAlmanacDataIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

instance (MonadIO m, Algebra sig m) => Algebra (AlmanacData :+: sig) (AlmanacDataIOC m) where
  alg hdl sig ctx = case sig of
    L (RunExactQuery q) -> (<$ ctx) <$> liftIO (runQuery q >>= eventsWithExactitude)
    R other -> AlmanacDataIOC (alg (runAlmanacDataIO . hdl) other ctx)
