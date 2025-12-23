{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (elemIndex, elemIndices, find, findIndices)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S

data BeamState = BeamState {cols :: S.Set Int, splits :: Int} deriving (Show)

beamReduce :: BeamState -> String -> BeamState
beamReduce BeamState {cols, splits} xs =
  let splitters = S.fromList $ elemIndices '^' xs
      collisions = S.intersection cols splitters
      newBeams = S.fromList $ concat [[c - 1, c + 1] | c <- S.toList collisions]
      oldBeams = S.difference cols collisions
      allBeams = S.union newBeams oldBeams
   in BeamState {cols = allBeams, splits = splits + length collisions}

part1 :: [String] -> Int
part1 (fstLine : rest) =
  let start = S.fromList $ [fromJust $ elemIndex 'S' fstLine]
      x = foldl beamReduce BeamState {cols = start, splits = 0} rest
   in splits x

-- Memoized version using explicit cache
type Cache = M.Map (Int, Int) Int

quantumBounce :: Int -> [String] -> Int
quantumBounce start ls = fst $ quantumBounce' start 0 M.empty
  where
    lsArray = ls -- Keep reference to original list
    quantumBounce' :: Int -> Int -> Cache -> (Int, Cache)
    quantumBounce' b lineNum cache =
      case M.lookup (b, lineNum) cache of
        Just result -> (result, cache)
        Nothing ->
          let collisionLines = dropWhile (\l -> (l !! b) /= '^') (drop lineNum lsArray)
              hit = take 1 collisionLines
              droppedLines = lineNum + length (drop lineNum lsArray) - length collisionLines + 1
           in if null hit
                then (1, M.insert (b, lineNum) 1 cache)
                else
                  let (leftResult, cache1) = quantumBounce' (b - 1) droppedLines cache
                      (rightResult, cache2) = quantumBounce' (b + 1) droppedLines cache1
                      result = leftResult + rightResult
                   in (result, M.insert (b, lineNum) result cache2)

part2 :: [String] -> Int
part2 (fstLine : rest) =
  let start = fromJust $ elemIndex 'S' fstLine
   in quantumBounce start rest

main :: IO ()
main = do
  ls <- lines <$> readFile "./data/day07.txt"
  print $ part1 ls
  print $ part2 ls
