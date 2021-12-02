module Day02 (run) where

import Common (readLines)
import Control.Applicative ((<|>))
import Data.Foldable (Foldable (foldl'))
import Data.List (stripPrefix)
import Text.Read (readEither)

data Movement = Forward Int | Down Int | Up Int deriving (Show)

parseMovement :: String -> Either String Movement
parseMovement line =
  case words line of
    ["forward", num] -> Forward <$> readEither num
    ["down", num] -> Down <$> readEither num
    ["up", num] -> Up <$> readEither num
    _ -> Left ("unknown movement: " ++ line)

type Horizontal = Int

type Depth = Int

move :: (Horizontal, Depth) -> Movement -> (Horizontal, Depth)
move (h, d) (Forward n) = (h + n, d)
move (h, d) (Down n) = (h, d + n)
move (h, d) (Up n) = (h, d - n)

type Aim = Int

moveWithAim :: (Horizontal, Depth, Aim) -> Movement -> (Horizontal, Depth, Aim)
moveWithAim (h, d, a) (Forward n) = (h + n, d + (n * a), a)
moveWithAim (h, d, a) (Down n) = (h, d, a + n)
moveWithAim (h, d, a) (Up n) = (h, d, a - n)

run :: IO ()
run = do
  result <- traverse parseMovement <$> readLines "./input/day02"

  case result of
    Left err -> putStrLn $ "parse error: " ++ err
    Right movements -> do
      let (h, d) = foldl' move (0, 0) movements
      putStrLn $ "move w/o aim: " ++ show (h * d)

      let (h, d, _) = foldl' moveWithAim (0, 0, 0) movements
      putStrLn $ "move w/ aim: " ++ show (h * d)
