module Common (readLines, last', head') where

import Data.Text (Text)
import qualified Data.Text.IO as Text

readLines :: String -> IO [String]
readLines = fmap lines . readFile

last' :: [a] -> Maybe a
last' [x] = Just x
last' (_ : xs) = last' xs
last' [] = Nothing

head' :: [a] -> Maybe a
head' (x : _) = Just x
head' _ = Nothing
