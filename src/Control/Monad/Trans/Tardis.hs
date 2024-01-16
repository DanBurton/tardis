{-# LANGUAGE RecursiveDo                     #-}
{-# LANGUAGE TupleSections #-}

-- | The data definition of a "TardisT"
-- as well as its primitive operations,
-- and straightforward combinators based on the primitives.
--
-- See Control.Monad.Tardis for the general explanation
-- of what a Tardis is and how to use it.
module Control.Monad.Trans.Tardis (
    -- * The Tardis monad transformer
    TardisT (TardisT, runTardisT)
  , evalTardisT
  , execTardisT

    -- * The Tardis monad
  , Tardis
  , runTardis
  , evalTardis
  , execTardis

    -- * Primitive Tardis operations
  , tardis

  , getPast
  , getFuture
  , sendPast
  , sendFuture

    -- * Composite Tardis operations
  , modifyForwards
  , modifyBackwards

  , getsPast
  , getsFuture

    -- * Other
  , mapTardisT
  , liftTardisT
  , noState
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.Morph


-- Definition
-------------------------------------------------

-- | A TardisT is parameterized by two state streams:
-- a 'backwards-traveling' state and a 'forwards-traveling' state.
-- This library consistently puts the backwards-traveling state first
-- whenever the two are seen together.
newtype TardisT bw fw m a = TardisT
  { runTardisT :: (bw, fw) -> m (a, (bw, fw))
    -- ^ A TardisT is merely an effectful state transformation
  }

-- | Using a Tardis with no monad underneath
-- will prove to be most common use case.
-- Practical uses of a TardisT require that the
-- underlying monad be an instance of MonadFix,
-- but note that the IO instance of MonadFix
-- is almost certainly unsuitable for use with
-- Tardis code.
type Tardis bw fw = TardisT bw fw Identity

-- | A Tardis is merely a pure state transformation.
runTardis :: Tardis bw fw a -> (bw, fw) -> (a, (bw, fw))
runTardis m = runIdentity . runTardisT m


-- Helpers
-------------------------------------------------

-- | Run a Tardis, and discard the final state,
-- observing only the resultant value.
evalTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m a
evalTardisT t s = fst `liftM` runTardisT t s

-- | Run a Tardis, and discard the resultant value,
-- observing only the final state (of both streams).
-- Note that the 'final' state of the backwards-traveling state
-- is the state it reaches by traveling from the 'bottom'
-- of your code to the 'top'.
execTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m (bw, fw)
execTardisT t s = snd `liftM` runTardisT t s


-- | Run a Tardis, and discard the final state,
-- observing only the resultant value.
evalTardis :: Tardis bw fw a -> (bw, fw) -> a
evalTardis t = runIdentity . evalTardisT t

-- | Run a Tardis, and discard the resultant value,
-- observing only the final state (of both streams).
execTardis :: Tardis bw fw a -> (bw, fw) -> (bw, fw)
execTardis t = runIdentity . execTardisT t

-- | An action in the underlying monad (or just functor, really)
-- can also be used in a Tardis by lifting it in.
liftTardisT :: (Functor m) => m a -> TardisT bw fw m a
liftTardisT m = TardisT $ \s -> fmap (,s) m

-- | A function that operates on the internal representation of a Tardis
-- can also be used on a Tardis.
mapTardisT :: (m (a, (bw, fw)) -> n (b, (bw, fw)))
           -> TardisT bw fw m a -> TardisT bw fw n b
mapTardisT f m = TardisT $ f . runTardisT m

-- | Some Tardises never observe the 'initial' state
-- of either state stream, so it is convenient
-- to simply hand dummy values to such Tardises.
-- 
-- > noState = (undefined, undefined)
noState :: (a, b)
noState = (undefined, undefined)


-- Instances
-------------------------------------------------

instance MonadFix m => Monad (TardisT bw fw m) where
  return x = tardis $ \s -> (x, s)
  m >>= f  = TardisT $ \ ~(bw, fw) -> do
    rec (x,  ~(bw'', fw' )) <- runTardisT m (bw', fw)
        (x', ~(bw' , fw'')) <- runTardisT (f x) (bw, fw')
    return (x', (bw'', fw''))

instance MonadFix m => Functor (TardisT bw fw m) where
  fmap = liftM

instance MonadFix m => Applicative (TardisT bw fw m) where
  pure = return
  (<*>) = ap

instance MonadFix m => MonadFix (TardisT bw fw m) where
  mfix f = TardisT $ \s -> do
    rec (x, s') <- runTardisT (f x) s
    return (x, s')

instance MFunctor (TardisT bw fw) where
  hoist f = mapTardisT f

-- Basics
-------------------------------------------------

-- | From a stateful computation, construct a Tardis.
-- This is the pure parallel to the constructor "TardisT",
-- and is polymorphic in the transformed monad.
tardis :: Monad m => ((bw, fw) -> (a, (bw, fw))) -> TardisT bw fw m a
tardis f = TardisT $ \s -> return (f s)

-- | Retrieve the current value of the 'forwards-traveling' state,
-- which therefore came forwards from the past.
-- You can think of forwards-traveling state as traveling
-- 'downwards' through your code.
getPast :: Monad m => TardisT bw fw m fw
getPast = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))

-- | Retrieve the current value of the 'backwards-traveling' state,
-- which therefore came backwards from the future.
-- You can think of backwards-traveling state as traveling
-- 'upwards' through your code.
getFuture :: Monad m => TardisT bw fw m bw
getFuture = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))

-- | Set the current value of the 'backwards-traveling' state,
-- which will therefore be sent backwards to the past.
-- This value can be retrieved by calls to "getFuture"
-- located 'above' the current location,
-- unless it is overwritten by an intervening "sendPast".
sendPast :: Monad m => bw -> TardisT bw fw m ()
sendPast bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))

-- | Set the current value of the 'forwards-traveling' state,
-- which will therefore be sent forwards to the future.
-- This value can be retrieved by calls to "getPast"
-- located 'below' the current location,
-- unless it is overwritten by an intervening "sendFuture".
sendFuture :: Monad m => fw -> TardisT bw fw m ()
sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))


-- | Modify the forwards-traveling state
-- as it passes through from past to future.
modifyForwards :: MonadFix m => (fw -> fw) -> TardisT bw fw m ()
modifyForwards f = getPast >>= sendFuture . f

-- | Modify the backwards-traveling state
-- as it passes through from future to past.
modifyBackwards :: MonadFix m => (bw -> bw) -> TardisT bw fw m ()
modifyBackwards f = do
  rec
    sendPast (f x)
    x <- getFuture
  return ()


-- | Retrieve a specific view of the forwards-traveling state.
getsPast :: MonadFix m => (fw -> a) -> TardisT bw fw m a
getsPast f = fmap f getPast


-- | Retrieve a specific view of the backwards-traveling state.
getsFuture :: MonadFix m => (bw -> a) -> TardisT bw fw m a
getsFuture f = fmap f getFuture
