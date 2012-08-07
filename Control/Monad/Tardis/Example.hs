{-# LANGUAGE DoRec #-}

module Control.Monad.Tardis.Example where

import Control.Monad.Tardis

data BowlingGame = BowlingGame
  { frames :: ![Frame]  -- should be 9, too tedious to type restrict
  , lastFrame :: LFrame }

data Frame = Strike
           | Spare { firstThrow :: !Int }
           | Frame { firstThrow, secondThrow :: !Int }

data LFrame = LStrike { bonus1, bonus2 :: !Int }
            | LSpare { throw1, bonus1 :: !Int }
            | LFrame { throw1, throw2 :: !Int }

sampleGame = BowlingGame
  { frames =
    [ Strike    , Spare 9
    , Strike    , Strike
    , Strike    , Frame 8 1
    , Spare 7   , Strike
    , Strike
    ]
  , lastFrame = LStrike 10 10
  }

newtype PreviousScores = PreviousScores [Int]
newtype NextThrows = NextThrows (Int, Int)

toScores :: BowlingGame -> [Int]
toScores game = flip evalTardis initState $ go (frames game) where
  go :: [Frame] -> Tardis NextThrows PreviousScores [Int]
  go [] = do
    PreviousScores scores@(score : _) <- getPast
    return $ (finalFrameScore + score) : scores
  go (f : fs) = do
    rec
      sendPast $ NextThrows throws'
      PreviousScores scores@(score : _) <- getPast
      sendFuture $ PreviousScores (score' : scores)
      NextThrows ~(nextThrow1, nextThrow2) <- getFuture
      let (score', throws') = case f of
            Strike    -> (score + 10 + nextThrow1 + nextThrow2, (10, nextThrow1))
            Spare n   -> (score + 10 + nextThrow1,              (n, 10 - n))
            Frame n m -> (score + n + m,                        (n, m))
    go fs

  finalFrameScore = case lastFrame game of
    LStrike n m -> 10 + n + m
    LSpare n m  -> 10 + m
    LFrame n m -> n + m

  initState = (NextThrows $ case lastFrame game of
    LStrike n m -> (10, n)
    LSpare n _m -> (n, 10 - n)
    LFrame n m -> (n, m)
    , PreviousScores [0])


