{-# LANGUAGE
      DoRec
    , FlexibleInstances
    , MultiParamTypeClasses
    , FunctionalDependencies
    #-}

module Control.Tardis (
    TardisT
  , runTardisT
  , evalTardisT
  , execTardisT

  , Tardis
  , runTardis
  , evalTardis
  , execTardis
  
  , getPast
  , getFuture
  , sendPast
  , sendFuture
  
  , modifyForwards
  , modifyBackwards
  
  , noState
  ) where

import Control.Tardis.Internal

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.State

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


instance MonadFix m => Monad (TardisT bw fw m) where
  return x = tardis $ \s -> (x, s)
  m >>= f  = tardisT $ \ ~(bw, fw) -> do
    rec (x,  ~(bw'', fw' )) <- runTardisT m (bw', fw)
        (x', ~(bw' , fw'')) <- runTardisT (f x) (bw, fw')
    return (x', (bw'', fw''))

instance MonadFix m => Functor (TardisT bw fw m) where
  fmap = liftM

instance MonadFix m => Applicative (TardisT bw fw m) where
  pure = return
  (<*>) = ap

instance MonadTrans (TardisT bw fw) where
  lift m = tardisT $ \s -> do
    x <- m
    return (x, s)

instance MonadFix m => MonadFix (TardisT bw fw m) where
  mfix f = tardisT $ \s -> do
    rec (x, s') <- runTardisT (f x) s
    return (x, s')


class Monad m => MonadTardis bw fw m | m -> bw, m -> fw where
  getPast    :: m fw
  getFuture  :: m bw
  sendPast   :: bw -> m ()
  sendFuture :: fw -> m ()

instance MonadFix m => MonadTardis bw fw (TardisT bw fw m) where
  getPast        = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))
  getFuture      = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))
  sendPast   bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))
  sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))


modifyForwards :: MonadTardis bw fw m => (fw -> fw) -> m ()
modifyForwards f = getPast >>= sendFuture . f

modifyBackwards :: MonadTardis bw fw m => (bw -> bw) -> m ()
modifyBackwards f = getFuture >>= sendPast . f

instance MonadFix m => MonadState fw (TardisT bw fw m) where
  get = getPast
  put = sendFuture
