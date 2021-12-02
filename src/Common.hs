module Common (readLines) where

readLines :: String -> IO [String]
readLines = fmap lines . readFile
