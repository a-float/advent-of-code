{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Function (on)
import Data.List (groupBy, minimumBy, nub, sortBy)
import Data.Set qualified as S
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

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

getBounds :: [Point] -> (Point, Point)
getBounds ps = ((minimum xs - 1, minimum ys - 1), (maximum xs + 1, maximum ys + 1)) where (xs, ys) = unzip ps

getEdges ps =
  let pairs = zip ps $ drop 1 (cycle ps)
      makeSegment ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
   in S.fromList $ concat $ map makeSegment pairs

drawFloor :: (Point -> Char) -> [Point] -> IO ()
drawFloor getChar ps = do
  let bounds = getBounds ps
  let edges = getEdges ps
  let ((minX, minY), (maxX, maxY)) = bounds
  let output = unlines [[getChar (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  putStr output

-- returns visited set and list of points to visit
getOutside :: Point -> (Point, Point) -> S.Set Point -> [(S.Set Point, [Point])]
getOutside start bounds@((minX, minY), (maxX, maxY)) walls =
  let stepBfs (visited, []) = (visited, [])
      stepBfs (visited, (current@(x, y) : rest))
        | S.member current visited = (visited, rest)
        | otherwise =
            let neighs = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]
                hasWallNeigh = any (\x -> S.member x walls) neighs
                okNeighs = filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY && S.notMember (x, y) walls) neighs
             in if not hasWallNeigh then (visited, rest) else (S.insert current visited, nub $ rest ++ okNeighs)
   in iterate stepBfs ((S.empty), [start])

getRectEdge :: (Point, Point) -> [Point]
getRectEdge ((x1, y1), (x2, y2)) = concat [makeSegment ((x1, y1), (x1, y2)), makeSegment ((x1, y2), (x2, y2)), makeSegment ((x2, y2), (x2, y1)), makeSegment ((x1, y2), (x1, y1))]
  where
    makeSegment ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

isOnRectEdge :: (Point, Point) -> Point -> Bool
isOnRectEdge ((x1, y1), (x2, y2)) (x, y) =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
   in ((y == y1 || y == y2) && x >= minX && x <= maxX) || ((x == x1 || x == x2) && y >= minY && y <= maxY)

checkRect :: p -> S.Set Point -> (Point, Point) -> (Int, Bool)
checkRect corners out rect =
  let size = uncurry rectSize rect
      edge = getRectEdge rect
      isOk = all (\p -> S.notMember p out) edge
   in (size, isOk)

main :: IO ()
main = do
  let filename = "./data/day09.txt"
  corners <- map (toPoint . map read . splitOn ',') . lines <$> readFile filename
  let rects = sortBy (flip compare `on` uncurry rectSize) [(p1, p2) | p1 <- corners, p2 <- corners]
  print $ uncurry rectSize $ head rects
  let bounds = getBounds corners
  let cornerSet = S.fromList corners
  putStrLn $ "Bounds: " ++ show bounds

  let edges = getEdges corners
  let leftestCorner = minimumBy (compare `on` fst) corners
  let outIter = getOutside ((fst leftestCorner - 1, snd leftestCorner)) bounds edges
  -- cache previous result instead of speeding up the bfs
  let outFile = filename ++ "_cache"
  out <- do
    fileExists <- doesFileExist outFile
    if fileExists
      then do
        putStrLn "Loading cached out value..."
        read <$> readFile outFile
      else do
        putStrLn "Calculating out value..."
        let result = fst $ head $ filter (\(_, toVisit) -> null toVisit) outIter
        writeFile outFile (show result)
        return result
  -- drawFloor
  --   ( \p -> case p of
  --       _
  --         | S.member p edges -> 'X'
  --         | S.member p out -> 'o'
  --         | otherwise -> '.'
  --   )
  --   points

  putStrLn $ "Out calculated: " ++ (show $ S.take 1 out)
  let rectsLenght = length rects
  let skipRects = dropWhile (\r -> uncurry rectSize r > 1748851332) rects
  let (notOk, rest) = break (\(_, (size, ok)) -> ok) $ zip [1 ..] $ map (checkRect (corners) out) skipRects
  mapM_ print notOk

  putStrLn $ "Best: " ++ show (take 1 rest)
