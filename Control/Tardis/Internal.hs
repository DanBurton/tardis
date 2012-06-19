module Control.Tardis.Internal (
    TardisT
  , runTardisT
  , Tardis
  , runTardis
  , tardisT
  , tardis
  ) where

import Control.Monad.Identity

newtype TardisT bw fw m a = TardisT
  { runTardisT :: (bw, fw) -> m (a, (bw, fw)) }
type Tardis bw fw = TardisT bw fw Identity

runTardis :: Tardis bw fw a -> (bw, fw) -> (a, (bw, fw))
runTardis m = runIdentity . runTardisT m

tardisT :: ((bw, fw) -> m (a, (bw, fw))) -> TardisT bw fw m a
tardisT = TardisT

tardis :: Monad m => ((bw, fw) -> (a, (bw, fw))) -> TardisT bw fw m a
tardis f = TardisT $ \s -> return (f s)
