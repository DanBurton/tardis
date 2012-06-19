{-# LANGUAGE
      GeneralizedNewtypeDeriving
    , DoRec
    , FunctionalDependencies
    , FlexibleInstances
    #-}

module Control.Seer (
    Seer
  , runSeer
  , runSeerWith
  
  , SeerT
  , runSeerT
  , runSeerTWith

  , MonadSeer
  , contact
  , see
  , send
  ) where


import Control.Tardis

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Monoid


newtype SeerT s m a = SeerT (TardisT s s m a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans)
type Seer s = SeerT s Identity

runSeerTWith :: Monad m => s -> s -> SeerT s m a -> m a
runSeerTWith bw fw (SeerT t) = fst `liftM` runTardisT t (bw, fw)

runSeerT :: (Monad m, Monoid s) => SeerT s m a -> m a
runSeerT = runSeerTWith mempty mempty

runSeerWith :: s -> s -> Seer s a -> a
runSeerWith bw fw seer = runIdentity $ runSeerTWith bw fw seer

runSeer :: Monoid s => Seer s a -> a
runSeer = runSeerWith mempty mempty

class (Monoid s, Monad m, Functor m) => MonadSeer s m | m -> s where
  contact :: s -> m s
  contact s = send s >> see
  
  see :: m s
  see = contact mempty
  
  send :: s -> m ()
  send = void . contact


instance (Monoid s, MonadFix m) => MonadSeer s (SeerT s m) where
  contact s = SeerT $ do
    rec (past, future) <- do
         sendPast (s <> future)
         p <- getPast
         sendFuture (past <> s)
         f <- getFuture
         return (p, f)
    return $ past <> s <> future
