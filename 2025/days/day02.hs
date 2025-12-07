import Data.Text (pack, splitOn, unpack)

type Range = (Integer, Integer)

first (x : y : xs) = x : first xs
first (x : _) = [x]
first _ = []

second (x : y : xs) = y : second xs
second _ = []

makeRanges :: [String] -> [Range]
makeRanges xs = let nums :: [Integer] = map read xs in zip (first nums) (second nums)

------------------- PART 1

isValid x =
  let s = show x
      (a, b) = splitAt (length s `div` 2) s
   in a == b

part1 ranges =
  let steps = [x | (a, b) <- ranges, x <- [a .. b]]
   in sum $ filter isValid steps

------------------- PART 2

isValid2 x =
  let s = show x
      l = length s
   in or [s == (concat $ replicate times start) | i <- [1 .. (l - 1)], let times = div l i, let start = take i s]

part2 ranges =
  let steps = [x | (a, b) <- ranges, x <- [a .. b]]
   in sum $ filter isValid2 steps

main :: IO ()
main = do
  s <- readFile "./data/day02.txt"
  let x = [1 .. 2]
  let ranges = makeRanges $ map unpack $ concat . map (splitOn (pack "-")) $ splitOn (pack ",") (pack s)
  print ranges
  print $ part1 ranges
  print $ part2 ranges
