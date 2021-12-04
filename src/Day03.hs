module Day03 (run) where

import Common (readLines)
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Bits (Bits (complement, shift, (.&.), (.|.)))
import Data.Foldable (foldl')
import Data.Monoid (Sum (Sum))

toBits :: String -> [Bool]
toBits = map (== '1')

frequencies :: [[Bool]] -> [(Sum Int, Sum Int)]
frequencies = foldr1 (zipWith mappend) . fmap (fmap categorize)
  where
    categorize False = (Sum 1, Sum 0)
    categorize True = (Sum 0, Sum 1)

gamma :: (Sum Int, Sum Int) -> Bool
gamma (zeros, ones) = zeros <= ones

epsilon :: (Sum Int, Sum Int) -> Bool
epsilon = not . gamma

toNum :: [Bool] -> Int
toNum = snd . foldr (\bit (i, bits) -> (i + 1, updateBit i bit bits)) (0, 0)
  where
    updateBit i bit bits = shift (fromEnum bit) i .|. bits

rating :: ((Sum Int, Sum Int) -> Bool) -> [[Bool]] -> [[Bool]]
rating toBit nums = foldl' go nums [1 .. length nums]
  where
    go res pos
      | length res > 1 =
        let freq = drop (pos - 1) (take pos (toBit <$> frequencies res))
         in filter (and . zipWith (==) freq . drop (pos - 1)) res
      | otherwise = res

run :: IO ()
run = do
  lines <- fmap toBits <$> readLines "./input/day03"

  let freq = frequencies lines
  let g = toNum (fmap gamma freq)
  let e = toNum (fmap epsilon freq)

  putStrLn $ "gamma rate: " ++ show g
  putStrLn $ "epsilon rate: " ++ show e
  putStrLn $ "power consumption rate: " ++ show (g * e)

  let o2Rating = head $ toNum <$> rating gamma lines
  let co2Rating = head $ toNum <$> rating epsilon lines

  putStrLn $ "o2 rating: " ++ show o2Rating
  putStrLn $ "co2 rating: " ++ show co2Rating
  putStrLn $ "life support rating: " ++ show (o2Rating * co2Rating)
