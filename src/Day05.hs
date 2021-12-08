{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day05 (run) where

import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor (first))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as Read

type Point = (Int, Int)

type Line = (Point, Point)

run :: IO ()
run = do
  let filepath = "./input/day05"
  res <- parse . Text.lines <$> TIO.readFile filepath
  case res of
    Left err -> print ("error: " <> err)
    Right lines -> do
      putStrLn $ "intersections: " ++ show (countIntersections (filter orthogonal lines))
      putStrLn $ "intersections w/ diag: " ++ show (countIntersections lines)
      putStrLn $ Text.unpack (render 9 9 (pointMap lines))

render :: Int -> Int -> Map Point Int -> Text
render width height pointMap = Text.unlines rows
  where
    rows = fmap (Text.concat . row) [0 .. height]
    row y = fmap (`cell` y) [0 .. width]
    cell x y = maybe "." (Text.pack . show) (Map.lookup (x, y) pointMap)

parse :: [Text] -> Either Text [Line]
parse = mapM (parseLine . Text.words)
  where
    parseLine :: [Text] -> Either Text Line
    parseLine [p1, "->", p2] = (,) <$> parsePoint (Text.splitOn "," p1) <*> parsePoint (Text.splitOn "," p2)
    parseLine line = Left ("invalid line format: " <> Text.unwords line)

    parsePoint :: [Text] -> Either Text Point
    parsePoint [x, y] = first Text.pack ((,) <$> fmap fst (Read.decimal x) <*> fmap fst (Read.decimal y))
    parsePoint point = Left ("invalid point format: " <> Text.intercalate "," point)

countIntersections :: [Line] -> Int
countIntersections = Map.size . Map.filter (> 1) . pointMap

pointMap :: [Line] -> Map Point Int
pointMap = Map.fromListWith (+) . fmap (,1) . (=<<) points

points :: Line -> [Point]
points ((x1, y1), (x2, y2)) = zip (steps x1 x2) (steps y1 y2)
  where
    steps from to
      | from == to = repeat from
      | from > to = [from, (from -1) .. to]
      | otherwise = [from .. to]

orthogonal :: Line -> Bool
orthogonal ((x1, y1), (x2, y2)) = y1 == y2 || x1 == x2
