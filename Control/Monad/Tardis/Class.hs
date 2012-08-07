{-# OPTIONS_GHC -Wall               #-}
{-# LANGUAGE DoRec                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Tardis.Class
  ( MonadTardis (..)
  , modifyForwards
  , modifyBackwards
  , getsPast
  , getsFuture
  ) where

import Control.Applicative
import Control.Monad.Fix

class (Applicative m, MonadFix m) => MonadTardis bw fw m | m -> bw, m -> fw where
  getPast    :: m fw
  getFuture  :: m bw
  sendPast   :: bw -> m ()
  sendFuture :: fw -> m ()

  getPast        = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))
  getFuture      = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))
  sendPast   bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))
  sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))

  tardis :: ((bw, fw) -> (a, (bw, fw))) -> m a
  tardis f = do
    rec
      let (a, (future', past')) = f (future, past)
      sendPast future'
      past <- getPast
      future <- getFuture
      sendFuture past'
    return a


modifyForwards :: MonadTardis bw fw m => (fw -> fw) -> m ()
modifyForwards f = getPast >>= sendFuture . f

modifyBackwards :: MonadTardis bw fw m => (bw -> bw) -> m ()
modifyBackwards f = getFuture >>= sendPast . f

getsPast :: MonadTardis bw fw m => (fw -> a) -> m a
getsPast f = f <$> getPast

getsFuture :: MonadTardis bw fw m => (bw -> a) -> m a
getsFuture f = f <$> getFuture

