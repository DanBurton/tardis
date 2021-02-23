{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

-- | The class definition of a Tardis,
-- as well as a few straightforward combinators
-- based on its primitives.
--
-- See Control.Monad.Tardis for the general explanation
-- of what a Tardis is and how to use it.
module Control.Monad.Tardis.Class
  ( -- * The MonadTardis class
    MonadTardis (..)
    -- * Composite Tardis operations
  , modifyForwards
  , modifyBackwards
  , getsPast
  , getsFuture
  ) where

import Control.Applicative
import Control.Monad.Fix

import qualified Control.Monad.Trans.Tardis as T

-- | A Tardis is parameterized by two state streams:
-- a 'backwards-traveling' state and a 'forwards-traveling' state.
-- This library consistently puts the backwards-traveling state first
-- whenever the two are seen together.
-- 
-- Minimal complete definition:
-- ("tardis") or
-- ("getPast", "getFuture", "sendPast", and "sendFuture").
class (Applicative m, MonadFix m) => MonadTardis bw fw m | m -> bw, m -> fw where
  -- | Retrieve the current value of the 'forwards-traveling' state,
  -- which therefore came forwards from the past.
  -- You can think of forwards-traveling state as traveling
  -- 'downwards' through your code.
  getPast    :: m fw
  
  -- | Retrieve the current value of the 'backwards-traveling' state,
  -- which therefore came backwards from the future.
  -- You can think of backwards-traveling state as traveling
  -- 'upwards' through your code.
  getFuture  :: m bw
  
  -- | Set the current value of the 'backwards-traveling' state,
  -- which will therefore be sent backwards to the past.
  -- This value can be retrieved by calls to "getFuture"
  -- located 'above' the current location,
  -- unless it is overwritten by an intervening "sendPast".
  sendPast   :: bw -> m ()
  
  -- | Set the current value of the 'forwards-traveling' state,
  -- which will therefore be sent forwards to the future.
  -- This value can be retrieved by calls to "getPast"
  -- located 'below' the current location,
  -- unless it is overwritten by an intervening "sendFuture".
  sendFuture :: fw -> m ()

  getPast        = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))
  getFuture      = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))
  sendPast   bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))
  sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))

  -- | A Tardis is merely a pure state transformation.
  tardis :: ((bw, fw) -> (a, (bw, fw))) -> m a
  tardis f = do
    rec
      let (a, (future', past')) = f (future, past)
      sendPast future'
      past <- getPast
      future <- getFuture
      sendFuture past'
    return a

-- | Modify the forwards-traveling state
-- as it passes through from past to future.
modifyForwards :: MonadTardis bw fw m => (fw -> fw) -> m ()
modifyForwards f = getPast >>= sendFuture . f

-- | Modify the backwards-traveling state
-- as it passes through from future to past.
modifyBackwards :: MonadTardis bw fw m => (bw -> bw) -> m ()
modifyBackwards f = do
  rec
    sendPast (f x)
    x <- getFuture
  return ()

-- | Retrieve a specific view of the forwards-traveling state.
getsPast :: MonadTardis bw fw m => (fw -> a) -> m a
getsPast f = f <$> getPast

-- | Retrieve a specific view of the backwards-traveling state.
getsFuture :: MonadTardis bw fw m => (bw -> a) -> m a
getsFuture f = f <$> getFuture


instance MonadFix m => MonadTardis bw fw (T.TardisT bw fw m) where
  getPast    = T.getPast
  getFuture  = T.getFuture
  sendPast   = T.sendPast
  sendFuture = T.sendFuture
  tardis     = T.tardis
