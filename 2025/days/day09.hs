{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (groupBy)
import Data.Set qualified as S

type Point = (Int, Int)

toPoint :: [Int] -> Point
toPoint (a : b : _) = (a, b)

splitOn :: Char -> String -> [String]
splitOn c = foldr step [[]]
  where
    step x acc
      | x == c = [] : acc
      | otherwise = (x : head acc) : drop 1 acc

rectSize :: Point -> Point -> Int
rectSize (a, b) (c, d) = abs (1 + (c - a)) * (1 + abs (d - b))

getColoredTiles :: [Point]
getColoredTiles = []

main :: IO ()
main = do
  points <- map (toPoint . map read . splitOn ',') . lines <$> readFile "./data/day09.txt"
  -- print points
  print $ maximum $ rectSize <$> points <*> points