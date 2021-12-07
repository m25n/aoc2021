module Day04 (run) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.IntMap.Lazy as Map
import Data.List (find, scanl')
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Text.Parsec (Parsec, many, many1, runParser)
import qualified Text.Parsec.Char as Char
import Text.Parsec.Combinator (eof, many1, sepBy, sepBy1, sepEndBy1)
import Text.Read (readEither)

data Board = Board
  { -- | Reverse index from value to cells
    unmarked :: IntMap [(Int, Int)],
    -- | How many items in a row have been marked
    rows :: IntMap Int,
    -- | How may items in a column has been marked
    cols :: IntMap Int
  }
  deriving (Show)

coordinates :: Board -> Int -> [(Int, Int)]
coordinates board num = fromMaybe [] (Map.lookup num (unmarked board))

mark :: Int -> Board -> Board
mark num init = foldr go init (coordinates init num)
  where
    go (row, col) board =
      board
        { unmarked = Map.delete num (unmarked board),
          rows = Map.insertWith (+) row 1 (rows board),
          cols = Map.insertWith (+) col 1 (cols board)
        }

unmarkedSum :: Board -> Int
unmarkedSum = sum . Map.mapWithKey (\num positions -> num * length positions) . unmarked

isWinner :: Board -> Bool
isWinner status =
  Map.size (Map.filter (== 5) (rows status)) > 0
    || Map.size (Map.filter (== 5) (cols status)) > 0

playStep :: Int -> [Board] -> [Board]
playStep num = fmap (mark num)

playSteps :: Game -> [(Int, [Board])]
playSteps (Game nums boards) = zip nums (drop 1 $ scanl' (flip playStep) boards nums)

firstWinner :: [(Int, [Board])] -> Maybe (Int, Board)
firstWinner steps =
  join (find isJust (uncurry extractWinner <$> steps))
  where
    extractWinner :: Int -> [Board] -> Maybe (Int, Board)
    extractWinner num boards = (,) num <$> find isWinner boards

lastWinner :: [(Int, [Board])] -> Maybe (Int, Board)
lastWinner steps =
  join (find isJust (uncurry maybeLastWinner <$> zip (drop 1 rSteps) rSteps))
  where
    rSteps = reverse steps

    maybeLastWinner :: (Int, [Board]) -> (Int, [Board]) -> Maybe (Int, Board)
    maybeLastWinner (_, beforeBoards) (num, afterBoards) =
      (,) num . snd <$> find (uncurry (/=) . bimap isWinner isWinner) (zip beforeBoards afterBoards)

score :: Int -> Board -> Int
score num board = num * unmarkedSum board

data Game = Game [Int] [Board] deriving (Show)

fromMatrix :: [[Int]] -> Board
fromMatrix m = Board (mkUmarked m) IntMap.empty IntMap.empty
  where
    mkUmarked :: [[Int]] -> IntMap [(Int, Int)]
    mkUmarked rows = IntMap.fromList $ concat $ zipWith mkUmarkedRow [0 ..] rows
    mkUmarkedRow :: Int -> [Int] -> [(Int, [(Int, Int)])]
    mkUmarkedRow row cols = zipWith (\col num -> (num, [(row, col)])) [0 ..] cols

parser :: Parsec Text () Game
parser =
  Game
    <$> (parseInt `sepBy` Char.char ',')
    <* Char.endOfLine
    <* Char.endOfLine
    <*> (parseBingoBoard `sepEndBy1` Char.endOfLine)
    <* eof
  where
    parseInt :: Parsec Text () Int
    parseInt = many1 Char.digit >>= either fail return . readEither

    parseBingoLine :: Parsec Text () [Int]
    parseBingoLine = many (Char.char ' ') *> parseInt `sepBy1` many1 (Char.char ' ')

    parseBingoBoard :: Parsec Text () Board
    parseBingoBoard = fromMatrix <$> parseBingoLine `sepEndBy1` Char.endOfLine

run :: IO ()
run = do
  let input = "./input/day04"
  res <- runParser parser () input <$> Text.readFile input
  case res of
    Left err -> putStrLn $ "error: " ++ show err
    Right game -> do
      let first = uncurry score <$> firstWinner (playSteps game)
      putStrLn $ "first winner: " ++ show first
      let last = uncurry score <$> lastWinner (playSteps game)
      putStrLn $ "last winner: " ++ show last
