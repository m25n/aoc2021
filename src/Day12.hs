module Day12 (run) where

import Common (readLines)
import Control.Monad (filterM, forM_, join)
import Data.Char (isUpper)
import qualified Data.IntMap.Lazy as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Cave = String

type Path = [Cave]

parse :: String -> (Cave, Cave)
parse line = (takeWhile (/= '-') line, tail (dropWhile (/= '-') line))

type Graph = Map Cave (Set Cave)

big :: Cave -> Bool
big [] = False
big (c : _) = isUpper c

insert :: (Cave, Cave) -> Graph -> Graph
insert (l, r) =
  if l == "end" || r == "start"
    then insert (r, l)
    else
      let insertReverse =
            if r /= "end" && l /= "start"
              then Map.insertWith (<>) r (Set.singleton l)
              else id
       in insertReverse . Map.insertWith (<>) l (Set.singleton r)

part1 :: Map String Int -> Cave -> Bool
part1 visitCounts cave =
  let visitCount = fromMaybe 0 (Map.lookup cave visitCounts)
   in not (big cave) && visitCount == 1

part2 :: Map String Int -> Cave -> Bool
part2 visitCounts cave =
  let visitCount = fromMaybe 0 (Map.lookup cave visitCounts)
      smallVisitedTwice = Map.size (Map.filterWithKey (\cave visits -> not (big cave) && visits == 2) visitCounts) == 1
   in not (big cave) && (visitCount > 1 || (visitCount == 1 && smallVisitedTwice))

paths :: (Map String Int -> Cave -> Bool) -> Cave -> Graph -> [Path]
paths pred start graph = paths' (Map.empty :: Map String Int) start graph
  where
    paths' visitCounts parent graph =
      let children = fromMaybe Set.empty (Map.lookup parent graph)
          visitCounts' = Map.insertWith (+) parent 1 visitCounts
       in if pred visitCounts parent
            then []
            else
              if null children
                then [[parent]]
                else (parent :) <$> concatMap (\child -> paths' visitCounts' child graph) children

run :: IO ()
run = do
  graph <- foldr insert Map.empty . fmap parse <$> readLines "./input/day12"
  print graph
  let part1Paths = paths part1 "start" graph
  putStrLn ("part 1: " <> show (length part1Paths))
  let part2Paths = paths part2 "start" graph
  putStrLn ("part 2: " <> show (length part2Paths))
