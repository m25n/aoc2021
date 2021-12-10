module Day10 where

import Common (readLines)
import Data.List (sort, uncons)
import Data.Maybe (mapMaybe)

firstIllegal :: String -> Maybe Char
firstIllegal = loop ""
  where
    loop _ [] = Nothing
    loop expect (x : xs) =
      case x of
        '(' -> loop (')' : expect) xs
        '[' -> loop (']' : expect) xs
        '{' -> loop ('}' : expect) xs
        '<' -> loop ('>' : expect) xs
        end -> case uncons expect of
          Just (e, es) ->
            if e /= end
              then Just end
              else loop es xs
          Nothing ->
            Nothing

completeLine :: String -> Maybe String
completeLine = loop ""
  where
    loop expected [] = Just expected
    loop expect (x : xs) =
      case x of
        '(' -> loop (')' : expect) xs
        '[' -> loop (']' : expect) xs
        '{' -> loop ('}' : expect) xs
        '<' -> loop ('>' : expect) xs
        end -> case uncons expect of
          Just (e, es) ->
            if e /= end
              then Nothing
              else loop es xs
          Nothing ->
            Nothing

scoreIllegal :: String -> Int
scoreIllegal [] = 0
scoreIllegal (')' : xs) = 3 + scoreIllegal xs
scoreIllegal (']' : xs) = 57 + scoreIllegal xs
scoreIllegal ('}' : xs) = 1197 + scoreIllegal xs
scoreIllegal ('>' : xs) = 25137 + scoreIllegal xs
scoreIllegal (_ : xs) = 0 + scoreIllegal xs

scoreComplete :: Int -> String -> Int
scoreComplete acc [] = acc
scoreComplete acc (')' : xs) = scoreComplete (acc * 5 + 1) xs
scoreComplete acc (']' : xs) = scoreComplete (acc * 5 + 2) xs
scoreComplete acc ('}' : xs) = scoreComplete (acc * 5 + 3) xs
scoreComplete acc ('>' : xs) = scoreComplete (acc * 5 + 4) xs
scoreComplete acc (_ : xs) = scoreComplete (acc * 5) xs

middle :: [a] -> a
middle xs =
  let midpoint = (length xs `div` 2)
   in xs !! midpoint

run :: IO ()
run = do
  lines <- readLines "./input/day10"
  print (scoreIllegal (mapMaybe firstIllegal lines))
  print (middle (sort (fmap (scoreComplete 0) (mapMaybe completeLine lines))))
