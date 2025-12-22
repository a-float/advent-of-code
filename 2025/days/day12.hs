{-# OPTIONS_GHC -Wno-x-partial #-}

data Region = Region {width :: Int, height :: Int, presents :: [Int]} deriving (Show)

instance Read Region where
  readsPrec _ s = [(Region w h ps, rest)]
    where
      (wStr, s1) = break (== 'x') s
      w = read wStr
      (hStr, s2) = break (== ':') (drop 1 s1)
      h = read hStr
      psStr = drop 1 s2
      ps = map read (words psStr)
      rest = ""

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f [] = [[]] -- one empty group even with no splits
splitOn f (x : xs)
  | f x == True = [] : rest
  | otherwise = (x : head rest) : drop 1 rest
  where
    rest = splitOn f xs

canFitPresentsInRegion :: Region -> Bool
canFitPresentsInRegion (Region w h presents) = regionSize >= maxPresentsSize
  where
    regionSize = w * h
    -- each presents fits in a 3x3 block
    -- this is a very crude heuristic that works well enough
    maxPresentsSize = 9 * sum presents

part1 regions = length $ filter canFitPresentsInRegion regions

main :: IO ()
main = do
  ls <- lines <$> readFile "./data/day12.txt"
  let groups = splitOn null ls
  let regions :: [Region] = map read $ last groups
  print $ part1 regions