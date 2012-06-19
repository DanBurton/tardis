module Control.Seer.Example where

import Control.Seer
import Control.Monad.Trans (lift)

import qualified Data.Map as M
import Data.Map (Map, (!))



foo :: Seer [Int] [Int]
foo = do
  x <- see
  send [1, 2, x !! 6]
  send (replicate (x !! 2) 4)
  send [3]
  see
  
-- two valid solutions:
-- x !! 6 = 3
-- x !! 6 = 4


runFoo = runSeer foo

bar :: Seer (M.Map Int Char) String
bar = do
  m <- see
  send (M.singleton 1 $ succ (m ! 2))
  send (M.singleton 2 'c')
  return [m ! 1, m ! 2]

runBar = runSeer bar


baz :: Seer [String] [String]
baz = do
  x <- see
  send ["olleH"]
  send ["Dan"]
  send ["Bye"]
  send [reverse (x !! 0) ++ " " ++ (x !! 4)]
  send ["car"]
  see

runBaz = runSeer baz
