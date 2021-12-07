module Common (readLines) where

import Data.Text (Text)
import qualified Data.Text.IO as Text

readLines :: String -> IO [String]
readLines = fmap lines . readFile
