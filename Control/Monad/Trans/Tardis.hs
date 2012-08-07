{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DoRec #-}

module Control.Monad.Trans.Tardis (
    TardisT
  , runTardisT
  , evalTardisT
  , execTardisT

  , Tardis
  , runTardis
  , evalTardis
  , execTardis

  , tardis

  , getPast
  , getFuture
  , sendPast
  , sendFuture

  , modifyForwards
  , modifyBackwards

  , getsPast
  , getsFuture

  , noState
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans


-- Definition
-------------------------------------------------
newtype TardisT bw fw m a = TardisT
  { runTardisT :: (bw, fw) -> m (a, (bw, fw)) }
type Tardis bw fw = TardisT bw fw Identity

runTardis :: Tardis bw fw a -> (bw, fw) -> (a, (bw, fw))
runTardis m = runIdentity . runTardisT m


-- Helpers
-------------------------------------------------

evalTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m a
evalTardisT t s = fst `liftM` runTardisT t s

execTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m (bw, fw)
execTardisT t s = snd `liftM` runTardisT t s

evalTardis :: Tardis bw fw a -> (bw, fw) -> a
evalTardis t = runIdentity . evalTardisT t

execTardis :: Tardis bw fw a -> (bw, fw) -> (bw, fw)
execTardis t = runIdentity . execTardisT t


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


instance MonadTrans (TardisT bw fw) where
  lift m = TardisT $ \s -> do
    x <- m
    return (x, s)

instance MonadFix m => MonadFix (TardisT bw fw m) where
  mfix f = TardisT $ \s -> do
    rec (x, s') <- runTardisT (f x) s
    return (x, s')


-- Basics
-------------------------------------------------

tardis :: Monad m => ((bw, fw) -> (a, (bw, fw))) -> TardisT bw fw m a
tardis f = TardisT $ \s -> return (f s)


getPast :: Monad m => TardisT bw fw m fw
getPast = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))

getFuture :: Monad m => TardisT bw fw m bw
getFuture = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))

sendPast :: Monad m => bw -> TardisT bw fw m ()
sendPast bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))

sendFuture :: Monad m => fw -> TardisT bw fw m ()
sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))


modifyForwards :: MonadFix m => (fw -> fw) -> TardisT bw fw m ()
modifyForwards f = getPast >>= sendFuture . f

modifyBackwards :: MonadFix m => (bw -> bw) -> TardisT bw fw m ()
modifyBackwards f = getFuture >>= sendPast . f


getsPast :: MonadFix m => (fw -> a) -> TardisT bw fw m a
getsPast f = fmap f getPast

getsFuture :: MonadFix m => (bw -> a) -> TardisT bw fw m a
getsFuture f = fmap f getFuture

