module Lib
  ( day01,
  )
where

import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum, getSum))

readLines :: String -> IO [String]
readLines = fmap lines . readFile

type WindowSize = Int

countIncreasing :: WindowSize -> [Int] -> Int
countIncreasing n measurements =
  zipWith (>) (drop n measurements) measurements
    & foldMap (Sum . fromEnum)
    & getSum

day01 :: IO ()
day01 = do
  measurements <- fmap read <$> readLines "./input/day01"

  let increasingCount = countIncreasing 1 measurements
  putStrLn $ "Number of increasing measurements: " ++ show increasingCount

  let increasingTripleCount = countIncreasing 3 measurements
  putStrLn $ "Number of increasing triples: " ++ show increasingTripleCount
