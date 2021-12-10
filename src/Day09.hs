module Day09 (run) where

import Common (groupWith, head', readLines)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (partition, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Floor = Floor {heights :: Map (Int, Int) Int, width :: Int, height :: Int} deriving (Show)

mkFloor :: [[Int]] -> Floor
mkFloor input =
  let height = length input
      width = maybe 0 length (head' input)
      heights = Map.fromList $ do
        (row, cols) <- zip [0 ..] input
        (col, height) <- zip [0 ..] cols
        return ((row, col), height)
   in Floor heights width height

up :: (Int, Int) -> (Int, Int)
up (r, c) = (r -1, c)

down :: (Int, Int) -> (Int, Int)
down (r, c) = (r + 1, c)

left :: (Int, Int) -> (Int, Int)
left (r, c) = (r, c - 1)

right :: (Int, Int) -> (Int, Int)
right (r, c) = (r, c + 1)

adjacent :: (Int, Int) -> (Int, Int) -> Bool
adjacent (r0, c0) (r1, c1) = abs (r0 - r1) + abs (c0 - c1) == 1

localMin :: Floor -> (Int, Int) -> Bool
localMin (Floor heights _ _) coords =
  let h_self = fromMaybe 9 (Map.lookup coords heights)
      h_up = fromMaybe 9 (Map.lookup (up coords) heights)
      h_down = fromMaybe 9 (Map.lookup (down coords) heights)
      h_left = fromMaybe 9 (Map.lookup (left coords) heights)
      h_right = fromMaybe 9 (Map.lookup (right coords) heights)
   in all (> h_self) [h_up, h_down, h_left, h_right]

findMins :: Floor -> [(Int, Int)]
findMins f@(Floor _ width height) =
  let coords = [(r, c) | r <- [0 .. (height - 1)], c <- [0 .. (width -1)]]
   in filter (localMin f) coords

risks :: Floor -> [(Int, Int)] -> [Int]
risks floor = fmap (maybe 0 (+ 1) . flip Map.lookup (heights floor))

totalRisk :: Floor -> [(Int, Int)] -> Int
totalRisk floor = sum . risks floor

isBasin :: Floor -> (Int, Int) -> Bool
isBasin (Floor heights _ _) = (< 9) . fromMaybe 9 . flip Map.lookup heights

rowBasins :: Floor -> Int -> [Set (Int, Int)]
rowBasins f@(Floor heights width _) row =
  let coords = zip (repeat row) [0 .. (width - 1)]
   in Set.fromList <$> groupWith (not . isBasin f) coords

basins :: Floor -> [Set (Int, Int)]
basins floor =
  sortBy (\a b -> compare (Set.size b) (Set.size a)) $
    foldl go [] (rowBasins floor <$> [0 .. (height floor - 1)])
  where
    go [] bs = bs
    go as [] = as
    go as bs@(b : bs') =
      let (notMatches, matches) = partition (Set.null . Set.intersection (Set.map up b)) as
       in if not (null matches)
            then go (Set.union (foldl Set.union Set.empty matches) b : notMatches) bs'
            else go as bs' ++ [b]

run :: IO ()
run = do
  floor <- mkFloor . fmap (fmap digitToInt) <$> readLines "./input/day09"
  putStrLn ("total risk: " ++ show (totalRisk floor (findMins floor)))
  putStrLn ("product of basins: " ++ show (product (length <$> take 3 (basins floor))))
