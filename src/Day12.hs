module Day12 (run) where

import Common (readLines)
import Control.Monad (filterM, forM_, join)
import Data.Char (isUpper)
import qualified Data.IntMap.Lazy as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

type Cave = String

big :: Cave -> Bool
big [] = False
big (c : _) = isUpper c

start :: Cave -> Bool
start = (== "start")

end :: Cave -> Bool
end = (== "end")

type Path = [Cave]

parse :: String -> (Cave, Cave)
parse line = (takeWhile (/= '-') line, tail (dropWhile (/= '-') line))

type Graph = Map Cave (Set Cave)

insert :: (Cave, Cave) -> Graph -> Graph
insert (l, r) =
  if start r || end l
    then insert (r, l)
    else insertReverse . insertForward
  where
    insertForward =
      Map.insertWith (<>) l (Set.singleton r)
    insertReverse =
      if not (start l || end r)
        then Map.insertWith (<>) r (Set.singleton l)
        else id

near :: Cave -> Graph -> [Cave]
near cave = maybe [] Set.toList . Map.lookup cave

data Multiverse a = Point a [Multiverse a]

value :: Multiverse a -> a
value (Point a _) = a

buildCaveVerse :: Graph -> Cave -> Multiverse Cave
buildCaveVerse graph cave =
  Point cave (fmap (buildCaveVerse graph) (near cave graph))

paths :: (s -> Multiverse a -> Maybe (Multiverse a, s)) -> Multiverse a -> s -> [[a]]
paths navigate (Point cave possibilities) state =
  if null possibilities
    then [[cave]]
    else (cave :) <$> (uncurry (paths navigate) =<< mapMaybe (navigate state) possibilities)

part1 :: Map String Int -> Multiverse Cave -> Maybe (Multiverse Cave, Map String Int)
part1 visitCounts p@(Point cave _) =
  if big cave || fromMaybe 0 (Map.lookup cave visitCounts) < 1
    then Just (p, Map.insertWith (+) cave 1 visitCounts)
    else Nothing

part2 :: Map String Int -> Multiverse Cave -> Maybe (Multiverse Cave, Map String Int)
part2 visitCounts p@(Point cave _) =
  let smallCavesVisitedMoreThanOnce = Map.size (Map.filterWithKey (\cave visits -> not (big cave) && visits > 1) visitCounts)
      visits cave = fromMaybe 0 . Map.lookup cave
   in if big cave || visits cave visitCounts < 1 || smallCavesVisitedMoreThanOnce == 0
        then Just (p, Map.insertWith (+) cave 1 visitCounts)
        else Nothing

run :: IO ()
run = do
  graph <- foldr insert Map.empty . fmap parse <$> readLines "./input/day12"
  let cv = buildCaveVerse graph "start"

  let part1Paths = paths part1 cv Map.empty
  putStrLn ("part 1: " <> show (length part1Paths))

  let part2Paths = paths part2 cv Map.empty
  putStrLn ("part 2: " <> show (length part2Paths))
