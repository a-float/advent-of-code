import Data.Char (isSpace)
import Data.List (groupBy, transpose)

calculate :: [String] -> Integer
calculate xs
  | last xs == "+" = sum nums
  | otherwise = product nums
  where
    nums :: [Integer] = map read $ init xs

part1 :: [[String]] -> Integer
part1 m = sum $ map calculate m

calculateCephalopods :: [String] -> Integer
calculateCephalopods xs
  | last xs == "+" = sum nums
  | otherwise = product nums
  where
    nums :: [Integer] = map read $ init xs

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty xs =
  let groups = groupBy (\a b -> notEmpty a && notEmpty b) xs
   in filter (not . null) $ map (filter notEmpty) groups
  where
    notEmpty = not . all (== ' ')

part2 :: [String] -> Integer
part2 xs =
  let cols = transpose (init xs)
      ops = words $ last xs
      groups = splitOnEmpty cols
      blocks = zipWith (\a b -> a ++ [b]) groups ops
   in part1 blocks

main :: IO ()
main = do
  lines <- lines <$> readFile "./data/day06.txt"
  let worded = map words lines
  let matrix = transpose worded

  print $ part1 matrix
  print $ part2 lines
