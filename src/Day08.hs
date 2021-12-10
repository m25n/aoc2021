{-# LANGUAGE OverloadedStrings #-}

module Day08 (run) where

import Common (head', last')
import Control.Monad (join, (<=<))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', forM_)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as Read

type Segment = Char

type Digit = Set Segment

run :: IO ()
run = do
  let filepath = "./input/day08"
  lines <- parse <$> TIO.readFile filepath
  putStrLn ("Digits 1, 4, 7, 8 appear " ++ show (solve1 lines) ++ " times")
  putStrLn ("Adding up all the output values gives " ++ show (sum (solve2 lines)))

parse :: Text -> [([Digit], [Digit])]
parse txt =
  Text.lines txt
    & mapMaybe parseLine
    & fmap (bimap parseSection parseSection)
  where
    parseLine :: Text -> Maybe (Text, Text)
    parseLine line =
      let parts = Text.splitOn "|" line
       in (,) <$> head' parts <*> last' parts
    parseSection :: Text -> [Digit]
    parseSection =
      fmap (Set.fromList . Text.unpack) . Text.words

solve1 :: [([Digit], [Digit])] -> Int
solve1 = sum . fmap (length . filter (\v -> v == 1 || v == 4 || v == 7 || v == 8)) . digitValues

solve2 :: [([Digit], [Digit])] -> [Int]
solve2 lines = fmap digitValuesToInt (digitValues lines)

-- | Map from sum of the segment frequencies for a number to that number
-- | Reddit user mnufat17 noticed that the sum of the frequencies of the
-- | segments for a digit are unique. And since the all possible digits on the
-- | left hand side of the pipe occur exactly once, it's possible to use this to
-- | figure out which digit is which.
-- |
-- | Segment frequencies
-- |  8888
-- | 6    8
-- | 6    8
-- |  7777
-- | 4    9
-- | 4    9
-- |  7777
digitIds :: IntMap Int
digitIds =
  IntMap.fromList
    [ (8 + 6 + 8 + 4 + 9 + 7, 0),
      (8 + 9, 1),
      (8 + 8 + 7 + 4 + 7, 2),
      (8 + 8 + 7 + 9 + 7, 3),
      (6 + 8 + 7 + 9, 4),
      (8 + 6 + 7 + 9 + 7, 5),
      (8 + 6 + 7 + 4 + 9 + 7, 6),
      (8 + 8 + 9, 7),
      (8 + 6 + 8 + 7 + 4 + 9 + 7, 8),
      (8 + 6 + 8 + 7 + 9 + 7, 9)
    ]

digitValues :: [([Digit], [Digit])] -> [[Int]]
digitValues lines = do
  (left, right) <- lines
  let segmentFrequencies = Map.fromListWith (+) (zip (left >>= Set.toList) (repeat 1))
  let digitToIds = Map.fromList (fmap (\d -> (d, sum (mapMaybe (`Map.lookup` segmentFrequencies) (Set.toList d)))) left)
  return $ fmap (fromMaybe 0 . (flip IntMap.lookup digitIds <=< flip Map.lookup digitToIds)) right

digitValuesToInt :: [Int] -> Int
digitValuesToInt = foldl' (\acc v -> acc * 10 + v) 0
