{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (findIndex, groupBy, sort)

type Range = (Integer, Integer)

data Edge = Open Integer | Close Integer deriving (Show, Eq)

getEdgeValue :: Edge -> Integer
getEdgeValue (Open x) = x
getEdgeValue (Close x) = x

instance Ord Edge where
  compare a b = compare (getEdgeValue a) (getEdgeValue b)

buildRange :: String -> Range
buildRange s = let (a, b) = break (== '-') s in (read a, read (tail b))

isFresh :: Integer -> [Range] -> Bool
isFresh i db = any (\(from, to) -> from <= i && i <= to) db

part1 :: [Range] -> [Integer] -> Int
part1 rs qs = length $ filter (flip isFresh rs) qs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n ls
  | n > 0 = (take n ls) : (chunk n $ drop n ls)
  | otherwise = error "Negative chunk size"

reduceEdge :: ([Edge], Int) -> Edge -> ([Edge], Int)
reduceEdge (((Close a) : xs'), d) (Open b) | a == b = (xs', d + 1)
reduceEdge (xs, 0) e@(Open _) = (e : xs, 1)
reduceEdge (xs, 1) e@(Close _) = (e : xs, 0)
reduceEdge (xs, d) (Open _) = (xs, d + 1)
reduceEdge (xs, d) (Close _) = (xs, d - 1)

part2 rs =
  let flat = sort $ concat [[Open a, Close b] | (a, b) <- rs]
      minEdges = reverse . fst $ foldl reduceEdge ([], 0) flat
      total = sum . map (\(a : b : _) -> b - a + 1) $ chunk 2 $ map getEdgeValue minEdges
   in total

-- First approach, very messy. Hard to implement case on line 32.
-- part2 rs =
--   let flat = sort $ concat [[Open a, Close b] | (a, b) <- rs]
--       depths =
--         scanl
--           ( \acc e -> case e of
--               Open _ -> acc + 1
--               Close _ -> acc - 1
--           )
--           0
--           flat
--       minEdges =
--         map fst
--           $ filter
--             ( \e -> case e of
--                 (Open _, 0) -> True
--                 (Close _, 1) -> True
--                 _ -> False
--             )
--           $ zip flat depths
--       grouped = groupBy (\a b -> getEdgeValue a == getEdgeValue b) minEdges
--       finalRanges = concat $ filter ((/= 2) . length) grouped
--    in sum $ map (\(a : b : _) -> 1 + getEdgeValue b - getEdgeValue a) $ chunk 2 finalRanges

main = do
  lines <- lines <$> readFile "./data/day05.txt"
  let Just idx = findIndex (\x -> length x == 0) lines
  let ranges = map buildRange $ filter (elem '-') lines
  let queries :: [Integer] = map read $ filter (\x -> length x > 0 && notElem '-' x) lines

  print $ part1 ranges queries
  print $ part2 ranges
