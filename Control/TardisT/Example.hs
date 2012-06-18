module Control.TardisT.Example where

import Control.TardisT

foo :: Tardis Int Int Int
foo = do
  x <- getFuture
  sendFuture 3
  y <- getPast
  sendPast 7
  return (x - y)

runFoo :: Int
runFoo = evalTardis foo noState
