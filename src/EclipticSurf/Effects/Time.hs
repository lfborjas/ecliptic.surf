
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module EclipticSurf.Effects.Time where

import Control.Algebra
import Data.Time (UTCTime, getCurrentTime)
import Data.Kind
import Control.Monad.IO.Class

data Time (m :: Type -> Type) k where
  Now :: Time m UTCTime

now :: (Has Time sig m) => m UTCTime
now = send Now

newtype TimeIOC m a = TimeIOC {runTimeIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (Time :+: sig) (TimeIOC m)
  where
  alg hdl sig ctx = case sig of
    L Now -> (<$ ctx) <$> liftIO getCurrentTime
    R other -> TimeIOC $ alg (runTimeIO . hdl) other ctx
