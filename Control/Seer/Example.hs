module Control.Seer.Example where

import Control.Seer

import qualified Data.Map as M
import Data.Map (Map, (!))

foo :: Seer [Int] [Int]
foo = do
  x <- see
  send [3, x !! 3]
  send (replicate (x !! 0) 2)
  send [4, 8]
  send [5]
  see

runFoo = runSeer foo

bar :: Seer (M.Map Int Char) String
bar = do
  m <- see
  send (M.singleton 1 $ succ (m ! 2))
  send (M.singleton 2 'c')
  return [m ! 1, m ! 2]

runBar = runSeer bar
