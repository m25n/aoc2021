{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day06 (run) where

import Data.Foldable (forM_)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as Read

type Gen = IntMap Int

mkGen :: [Int] -> Gen
mkGen = IntMap.fromListWith (+) . fmap (,1)

step :: Gen -> Gen
step gen =
  let gen' = IntMap.mapKeys (subtract 1) gen
      births = fromMaybe 0 (IntMap.lookup (-1) gen')
   in gen'
        & IntMap.insertWith (+) 8 births
        & IntMap.insertWith (+) 6 births
        & IntMap.delete (-1)

type Days = Int

simulate :: Days -> Gen -> Gen
simulate days = (!! days) . iterate step

parseGen :: Text -> Either String Gen
parseGen = fmap mkGen . traverse (fmap fst . Read.decimal) . Text.splitOn ","

run :: IO ()
run = do
  let filepath = "./input/day06"
  res <- parseGen <$> TIO.readFile filepath
  case res of
    Left err -> print err
    Right firstGen -> do
      putStrLn ("population after 80 days: " ++ show (sum (simulate 80 firstGen)))
      putStrLn ("population after 256 days: " ++ show (sum (simulate 256 firstGen)))
