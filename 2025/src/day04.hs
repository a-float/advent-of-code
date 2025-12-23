{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Set qualified as S

type Point = (Int, Int)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = zip [0 ..] xs

getSet :: [[Char]] -> S.Set Point
getSet lines =
  let lst = [(i, j) | (i, line) <- zipWithIndex lines, (j, c) <- zipWithIndex line, c == '@']
   in S.fromList lst

countNeighbors :: Point -> S.Set Point -> Int
countNeighbors (x, y) grid =
  sum
    [ 1
    | deltaX <- [-1 .. 1],
      deltaY <- [-1 .. 1],
      (deltaX, deltaY) /= (0, 0),
      S.member (x + deltaX, y + deltaY) grid
    ]

canRemoveRoll :: Int -> Int -> Point -> S.Set Point -> Bool
canRemoveRoll w h p set = S.member p set && 4 > countNeighbors p set

getRemovableRolls :: Int -> Int -> S.Set Point -> [Point]
getRemovableRolls w h set =
  [ (x, y)
  | x <- [0 .. w],
    y <- [0 .. h],
    canRemoveRoll w h (x, y) set
  ]

getDimensions :: [[Char]] -> Point
getDimensions lines = (length $ head lines, length lines)

removeRolls :: (S.Set Point -> [Point]) -> S.Set Point -> S.Set Point
removeRolls f set = foldr S.delete set (f set)

part1 :: Int -> Int -> S.Set Point -> Int
part1 w h set = length $ getRemovableRolls w h set

part2 :: Int -> Int -> S.Set Point -> Int
part2 w h set =
  let f = getRemovableRolls w h
      g = iterate (removeRolls f) set
      final = head $ dropWhile (\s -> length (f s) /= 0) $ g
   in length set - length final

main :: IO ()
main = do
  ls <- lines <$> readFile "./data/day04.txt"
  let s = getSet ls
  let (w, h) = getDimensions ls
  print $ part1 w h s
  print $ part2 w h s