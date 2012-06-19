{-# LANGUAGE GeneralizedNewtypeDeriving, DoRec #-}

module Control.Seer (
    Seer
  , runSeer
  , runSeerWith
  , contact
  , see
  , send
  ) where


import Control.TardisT

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Monoid


newtype Seer s a = Seer (Tardis s s a)
  deriving (Functor, Applicative, Monad, MonadFix)

runSeerWith :: s -> s -> Seer s a -> a
runSeerWith fw bw (Seer t) = fst $ runTardis t (fw, bw)

runSeer :: Monoid s => Seer s a -> a
runSeer = runSeerWith mempty mempty

contact :: Monoid s => s -> Seer s s
contact s = Seer $ do
  rec (past, future) <- do
         sendPast (s <> future)
         p <- getPast
         sendFuture (past <> s)
         f <- getFuture
         return (p, f)
  return $ past <> s <> future

see :: Monoid s => Seer s s
see = contact mempty

send :: Monoid s => s -> Seer s ()
send s = void $ contact s
