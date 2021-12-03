module Day01 (run) where

import Common (readLines)
import Data.Function ((&))

type WindowSize = Int

countIncreasing :: WindowSize -> [Int] -> Int
countIncreasing n measurements =
  zip (drop n measurements) measurements
    & filter (uncurry (>))
    & length

run :: IO ()
run = do
  measurements <- fmap read <$> readLines "./input/day01"

  let increasingCount = countIncreasing 1 measurements
  putStrLn $ "Number of increasing measurements: " ++ show increasingCount

  let increasingTripleCount = countIncreasing 3 measurements
  putStrLn $ "Number of increasing triples: " ++ show increasingTripleCount
