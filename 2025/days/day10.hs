{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Exception
import Data.List (find, findIndex, partition, sort, sortBy, sortOn, splitAt, transpose, (\\))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Set qualified as S
import Language.Haskell.TH (gadtC)
import System.CPUTime
import Text.XHtml (red)

type Button = [Int]

type Joltage = [Int]

type Matrix = [[Double]]

data Machine = Machine {target :: [Int], buttons :: [Button], joltage :: Joltage} deriving (Show)

parseTarget :: String -> [Int]
parseTarget xs = map snd $ filter (\c -> fst c == '#') $ init $ zip xs [-1 ..]

toBinList :: Int -> [Int] -> [Int]
toBinList n xs = [if i `elem` xs then 1 else 0 | i <- [0, 1 .. n]]

parseJoltage :: String -> [Int]
parseJoltage xs = read $ "[" ++ (init $ tail xs) ++ "]"

lineToMachine :: String -> Machine
lineToMachine xs =
  let ws = words xs
      (target : rest) = ws
      n = length target - 3
      btns = map (\b -> toBinList n $ read $ '[' : (tail $ init b) ++ "]") $ init rest
      joltage = last rest
   in Machine (toBinList n $ parseTarget target) btns (parseJoltage joltage)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = map (\r -> x : r) subsets' ++ subsets' where subsets' = subsets xs

solveMachine :: Machine -> Int
solveMachine (Machine target buttons _) =
  let ss = sortOn length $ subsets buttons
      reduceSet [] = []
      reduceSet s = map (\x -> rem x 2) $ foldr1 (\a acc -> zipWith (+) a acc) s
   in length . head $ filter (\s -> target == reduceSet s) ss

fixZero x = if x == 0 then 0 else x

gaussJordan' :: Int -> Matrix -> Matrix
gaussJordan' row ms =
  let sorted = sortOn (map abs) ms
      cRow = ms !! row
      rest = take row ms ++ drop (row + 1) ms
   in case findIndex (\x -> abs x > 0.001) (init cRow) of
        Nothing -> ms
        Just col ->
          let normRow = map (\x -> x / (cRow !! col)) cRow
              normRest = [zipWith (-) rs diff | rs <- rest, rs /= cRow, let m = rs !! col, let diff = map (* m) normRow]
           in normRow : normRest

gaussJordan :: Matrix -> Matrix
gaussJordan ms =
  -- weird I need to recheck row here
  let reduced = foldl (\acc i -> if i < length acc then gaussJordan' i acc else acc) ms [0 .. length ms - 1]
      needed = filter (\m -> any (\x -> abs x > 0.001) m) reduced
   in reverse $ sortOn (map abs) needed

findFreeVariables :: Matrix -> [Int]
findFreeVariables m =
  let numCols = length (head m) - 1 -- exclude last column (constant term)
      pivotCols = findPivotColumns m
   in [0 .. numCols - 1] \\ pivotCols

findPivotColumns :: Matrix -> [Int]
findPivotColumns = mapMaybe findLeadingOne
  where
    findLeadingOne row = findIndex (\x -> abs x > 0.001) (init row)

isValidPressCount :: Double -> Bool
isValidPressCount x = abs (x - fromIntegral (round x)) < 0.001 && x >= -0.001

solvePivotColumn :: M.Map Int Double -> [Double] -> Maybe Double
solvePivotColumn known row =
  let (pivot : rest) = dropWhile (\(coef, idx) -> abs coef < 0.001) $ zip (init row) [0 ..]
      l = last row
      restSum = sum $ map (\(coef, idx) -> if coef == 0 then 0 else coef * (fromJust $ M.lookup idx known)) rest
      pivotValue = (l - restSum) / (fst pivot) -- should always be one anyway
   in if isValidPressCount pivotValue then Just pivotValue else Nothing

solveMatrix :: M.Map Int Double -> [[Double]] -> Maybe (M.Map Int Double)
solveMatrix known rows =
  let go knownVars [] = Just knownVars
      go knownVars (row : rest) = do
        pivotCol <- findPivotColumn row
        pivotValue <- solvePivotColumn knownVars row
        go (M.insert pivotCol pivotValue knownVars) rest
   in go known rows
  where
    findPivotColumn row = findIndex (\x -> abs x > 0.001) (init row)

generateFreeValues :: [Int] -> Double -> [M.Map Int Double]
generateFreeValues freeIndices maxValue =
  let combinations = sequence (replicate (length freeIndices) [0.0 .. maxValue])
   in map (M.fromList . zip freeIndices) combinations

solveMachine2 m =
  let matrix = zipWith (\btn j -> btn ++ [j]) (transpose $ buttons m) (joltage m)
      reduced = gaussJordan (map fromIntegral <$> matrix)
      free = findFreeVariables reduced
      maxConstant = maximum $ map last reduced
      freeValues = generateFreeValues free (maxConstant + 10)
      rawSolutions = map (M.toList . fromJust) $ filter (isJust) $ map (\vals -> solveMatrix vals reduced) freeValues
      solutions = map (\x -> map (round . snd) x) rawSolutions
      smallest = minimum $ map sum solutions
   in smallest

main :: IO ()
main = do
  machines <- map lineToMachine . lines <$> readFile "./data/day10.txt"
  print $ sum $ map solveMachine machines
  let all = drop 0 $ zip [1 ..] (map solveMachine2 machines)
  mapM_ print all
  print $ sum (map snd all)
