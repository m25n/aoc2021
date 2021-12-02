module Lib
  ( day01,
  )
where

import qualified Data.List as List
import Data.Maybe (fromMaybe)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

countIncreasing :: [Int] -> Int
countIncreasing measurements =
  List.length (filter id (zipWith (>) (drop 1 measurements) measurements))

sumLists :: [Int] -> [Int] -> [Int]
sumLists = zipWith (+)

day01 :: IO ()
day01 = do
  measurements <- fmap (read :: String -> Int) <$> readLines "./input/day01"

  let increasingCount = countIncreasing measurements
  putStrLn $ "Number of increasing measurements: " ++ show increasingCount

  let tripleSum = sumLists (drop 2 measurements) (sumLists (drop 1 measurements) measurements)
  let increasingTripleCount = countIncreasing tripleSum
  putStrLn $ "Number of increasing triples: " ++ show increasingTripleCount
