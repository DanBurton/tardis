{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module Control.SeerRW (
    Seer
  , runSeer  
  
  , SeerT
  , runSeerT

  , tell
  , ask
  , see
  , send

  ) where

import qualified Control.Seer as S
import Control.Seer (see, send)

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans


newtype SeerT s m a =
  SeerT { unSeerT :: ReaderT s (WriterT s m) a }
  deriving (Monad, Functor, Applicative, MonadFix)
type Seer s = SeerT s Identity

instance Monoid s => MonadTrans (SeerT s) where
  lift m = SeerT (lift (lift m))

instance (Monad m, Monoid s) => MonadWriter s (SeerT s m) where
  tell s    = SeerT $ lift $ tell s
  listen ma = SeerT $ ReaderT $ \s -> WriterT $ do
    ~(a, w) <- runSeerTWith s ma
    return ((a, w), w)
  pass maww = SeerT $ ReaderT $ \s -> WriterT $ do
    ~((a, f), w) <- runSeerTWith s maww
    return (a, (f w))

instance (Monad m, Monoid s) => MonadReader s (SeerT s m) where
  ask = SeerT ask
  local f (SeerT m) = SeerT $ withReaderT f m


runSeerTWith :: (Monad m, Monoid s) => s -> SeerT s m a -> m (a, s)
runSeerTWith s (SeerT m) = runWriterT $ runReaderT m s

runSeerT :: (MonadFix m, Monoid s) => SeerT s m a -> m a
runSeerT m = fst `liftM` mfix (\ ~(_, s) -> runSeerTWith s m)

runSeer :: Monoid s => Seer s a -> a
runSeer = runIdentity . runSeerT

instance (Monad m, Functor m, Monoid s) => S.MonadSeer s (SeerT s m) where
  see = ask
  send = tell
