module Main where

import Control.Monad (forM_, when)
import qualified Data.List as List
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  when (List.null args) (putStrLn "usage: stack run [dayID[ dayID[ dayID]]]")
  forM_ args $ \dayId -> do
    putStrLn $ "running " ++ dayId ++ ":"
    case dayId of
      "day01" -> Day01.run
      "day02" -> Day02.run
      "day03" -> Day03.run
      "day04" -> Day04.run
      "day05" -> Day05.run
      "day06" -> Day06.run
      "day07" -> Day07.run
      "day08" -> Day08.run
      "day09" -> Day09.run
      _ -> putStrLn $ "error: unknown day " ++ dayId
    putStrLn ""
