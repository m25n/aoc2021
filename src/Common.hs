module Common (readLines, last', head', groupWith) where

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

groupWith :: (a -> Bool) -> [a] -> [[a]]
groupWith pred xs = case dropWhile pred xs of
  [] -> []
  xs' -> g : groupWith pred xs''
    where
      (g, xs'') =
        break pred xs'
