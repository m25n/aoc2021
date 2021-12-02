module Day01 (run) where

import Common (readLines)
import Control.Applicative ((<|>))
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum, getSum))

type WindowSize = Int

countIncreasing :: WindowSize -> [Int] -> Int
countIncreasing n measurements =
  zipWith (>) (drop n measurements) measurements
    & foldMap (Sum . fromEnum)
    & getSum

run :: IO ()
run = do
  measurements <- fmap read <$> readLines "./input/day01"

  let increasingCount = countIncreasing 1 measurements
  putStrLn $ "Number of increasing measurements: " ++ show increasingCount

  let increasingTripleCount = countIncreasing 3 measurements
  putStrLn $ "Number of increasing triples: " ++ show increasingTripleCount
