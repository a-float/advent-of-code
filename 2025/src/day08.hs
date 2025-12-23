{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (find, reverse, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Set qualified as S

data Vertex = Vertex {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord, Read)

type Edge = (Vertex, Vertex)

listToVertex :: [Int] -> Vertex
listToVertex (x : y : z : _) = Vertex x y z

vertexToList :: Vertex -> [Int]
vertexToList v = [x v, y v, z v]

distSq :: Vertex -> Vertex -> Int
distSq v1 v2 = sum $ map (\x -> x * x) $ zipWith (-) (vertexToList v1) (vertexToList v2)

groupJunctions :: [Edge] -> [S.Set Vertex] -> [S.Set Vertex]
groupJunctions [] vs = vs
groupJunctions ((e1, e2) : es) vs =
  let a = fromJust $ find (elem e1) vs
      b = fromJust $ find (elem e2) vs
      total = S.union a b
      rest = filter (\s -> not (elem e1 s || elem e2 s)) vs
   in groupJunctions es (total : rest)

-- Returns a tuple of edges left to apply and final curcuits
groupUntilOne :: [Edge] -> [S.Set Vertex] -> ([Edge], [S.Set Vertex])
groupUntilOne [] vs = ([], vs)
groupUntilOne ((e1, e2) : es) vs
  | length vs == 1 = ((e1, e2) : es, vs)
  | otherwise =
      let a = fromJust $ find (elem e1) vs
          b = fromJust $ find (elem e2) vs
          total = S.union a b
          rest = filter (\s -> not (elem e1 s || elem e2 s)) vs
       in groupUntilOne es (total : rest)

part1 :: [Vertex] -> Int
part1 ws =
  let allPairs = [(v1, v2) | v1 <- ws, v2 <- ws, v1 < v2]
      sorted = sortBy (\(a, b) (c, d) -> compare (distSq a b) (distSq c d)) allPairs
      v = groupJunctions (take 1000 sorted) (map S.singleton ws)
   in product . take 3 . reverse . sort $ (map length v)

part2 :: [Vertex] -> Int
part2 ws =
  let allPairs = [(v1, v2) | v1 <- ws, v2 <- ws, v1 < v2]
      sorted = sortBy (\(a, b) (c, d) -> compare (distSq a b) (distSq c d)) allPairs
      edgesLeft = fst $ groupUntilOne sorted (map S.singleton ws)
      lastUsedEdge = head $ drop (length sorted - length edgesLeft - 1) sorted
      (e1, e2) = lastUsedEdge
   in (x e1) * (x e2)

main :: IO ()
main = do
  ws <- map (\l -> listToVertex . read $ "[" ++ l ++ "]") . lines <$> readFile "./data/day08.txt"
  print $ part1 ws
  print $ part2 ws
