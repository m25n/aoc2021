{-# LANGUAGE OverloadedStrings #-}
module Day07 (run) where

import Data.Foldable (forM_)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as Read

run :: IO ()
run = do
  let filepath = "./input/day07"
  res <- parse <$> TIO.readFile filepath
  case res of
    Left err -> print err
    Right xs -> do
      let bestLinear = solve linear xs
      putStrLn ("linear: pos=" ++ show bestLinear ++ ", cost=" ++ show (linear bestLinear xs))
      let bestExponential = solve exponential xs
      putStrLn ("exponential: pos=" ++ show bestExponential ++ ", cost=" ++ show (exponential bestExponential xs))

parse :: Text -> Either String [Int]
parse = traverse (fmap fst . Read.decimal) . Text.splitOn ","

linear :: Int -> [Int] -> Int
linear target = sum . map (abs . subtract target)

exponential :: Int -> [Int] -> Int
exponential target = sum . map (\n -> n * (n + 1) `div` 2) . map (abs . subtract target)

solve :: (Int -> [Int] -> Int) -> [Int] -> Int
solve cost xs =
  let smallest = foldr1 min xs
      largest = foldr1 max xs
   in foldr1 (\a b -> if cost a xs < cost b xs then a else b) [smallest .. largest]
