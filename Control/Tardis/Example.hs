module Control.Tardis.Example where

import Control.Tardis

foo :: Tardis Int Int Int
foo = do
  x <- getFuture
  sendFuture 3
  y <- getPast
  sendPast 7
  return (x - y)

runFoo :: Int
runFoo = evalTardis foo noState
