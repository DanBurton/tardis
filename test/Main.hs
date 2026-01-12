import Example
import System.Exit

main :: IO ()
main = case actualScores == expectedScores of
  False -> do
    putStrLn $ "Expected: " <> show expectedScores
    putStrLn $ "Actual: " <> show actualScores
    exitFailure
  True -> do
    putStrLn "Test passed."
    exitSuccess
