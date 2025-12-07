parseRotations :: [String] -> [Int]
parseRotations xs = [if dir == 'L' then -val else val | (dir : rest) <- xs, let val = read $ rest]

part1 :: (Integral a) => [a] -> Int
part1 = length . filter ((== 0) . (`rem` 100)) . scanl (+) 50

part2 :: (Integral a) => [a] -> Int
part2 xs =
  let steps = scanl (+) 50 xs
      pairs = zip steps (drop 1 steps)
      allSteps = concat $ [[a + diff, a + 2 * diff .. b] | (a, b) <- pairs, let diff = signum $ b - a]
      zeroSteps = length $ filter (\x -> rem x 100 == 0) $ allSteps
   in zeroSteps

main :: IO ()
main = do
  rots <- parseRotations . words <$> readFile "./data/day01.txt"
  print $ part1 rots
  print $ part2 rots
