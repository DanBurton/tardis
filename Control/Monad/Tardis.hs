{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE DoRec                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

#ifdef USE_UNDECIDABLE_INSTANCES
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverlappingInstances       #-}
#endif

module Control.Monad.Tardis
  ( module Control.Monad.Trans.Tardis
  , module Control.Monad.Tardis.Class
  ) where


#ifdef USE_UNDECIDABLE_INSTANCES
import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans, lift)
#endif

import Control.Monad.Fix
import Control.Monad.State.Class

import qualified Control.Monad.Trans.Tardis as T
import Control.Monad.Tardis.Class
import Control.Monad.Trans.Tardis
  ( TardisT
  , runTardisT
  , evalTardisT
  , execTardisT

  , Tardis
  , runTardis
  , evalTardis
  , execTardis
  
  , noState
  )

instance MonadFix m => MonadTardis bw fw (TardisT bw fw m) where
  getPast    = T.getPast
  getFuture  = T.getFuture
  sendPast   = T.sendPast
  sendFuture = T.sendFuture
  tardis     = T.tardis

instance MonadFix m => MonadState fw (TardisT bw fw m) where
  get = getPast
  put = sendFuture


#ifdef USE_UNDECIDABLE_INSTANCES
instance ( MonadTrans t
         , MonadTardis bw fw m
         , MonadFix (t m)
         , Applicative (t m)
         ) => MonadTardis bw fw (t m) where
  getPast    = lift getPast
  getFuture  = lift getFuture
  sendPast   = lift . sendPast
  sendFuture = lift . sendFuture
  tardis     = lift . tardis
#endif

