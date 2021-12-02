module Main where

import Control.Monad (forM_, when)
import qualified Data.List as List
import Lib
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  when (List.null args) (putStrLn "usage: stack run [dayID[ dayID[ dayID]]]")
  forM_ args $ \dayId -> do
    putStrLn $ "running " ++ dayId ++ ":"
    case dayId of
      "day01" -> day01
      _ -> putStrLn $ "error: unknown day " ++ dayId
    putStrLn ""
