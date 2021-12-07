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

score :: Int -> Board -> Int
score num board = num * unmarkedSum board

step :: [Board] -> Int -> [Board]
step boards num = fmap (mark num) boards

type Strategy =
  -- | board states before drawn number is applied
  [Board] ->
  -- | drawn number
  Int ->
  -- | board states after drawn number is applied
  [Board] ->
  -- | drawn number + winning board (if there is one)
  Maybe (Int, Board)

firstWinner :: Strategy
firstWinner before num after = do
  (_, winner) <- find (uncurry (/=) . bimap isWinner isWinner) (zip before after)
  return (num, winner)

lastWinner :: Strategy
lastWinner before num after = do
  winner <- firstWinner before num after
  if length (filter isWinner after) == length after - 1
    then return winner
    else Nothing

data Game = Game [Int] [Board] deriving (Show)

play :: Strategy -> Game -> Maybe (Int, Board)
play strategy (Game nums boards) =
  join (find isJust (zipWith3 strategy steps (drop 1 nums) (drop 1 steps)))
  where
    steps = drop 1 $ scanl' step boards nums

fromMatrix :: [[Int]] -> Board
fromMatrix rows = Board unmarked' IntMap.empty IntMap.empty
  where
    unmarked' = IntMap.fromListWith (++) $ do
      (row, cols) <- zip [0 ..] rows
      (col, num) <- zip [0 ..] cols
      return (num, [(row, col)])

parser :: Parsec Text () Game
parser =
  Game
    <$> (parseInt `sepBy` Char.char ',')
    <* Char.endOfLine
    <* Char.endOfLine
    <*> (parseBoard `sepEndBy1` Char.endOfLine)
  where
    parseInt :: Parsec Text () Int
    parseInt = many1 Char.digit >>= either fail return . readEither

    parseLine :: Parsec Text () [Int]
    parseLine = many (Char.char ' ') *> parseInt `sepBy1` many1 (Char.char ' ')

    parseBoard :: Parsec Text () Board
    parseBoard = fromMatrix <$> parseLine `sepEndBy1` Char.endOfLine

run :: IO ()
run = do
  let input = "./input/day04"
  res <- runParser parser () input <$> Text.readFile input
  case res of
    Left err -> putStrLn $ "error: " ++ show err
    Right game -> do
      let first = uncurry score <$> play firstWinner game
      putStrLn $ "first winner: " ++ show first
      let last = uncurry score <$> play lastWinner game
      putStrLn $ "last winner: " ++ show last
