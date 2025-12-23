import Data.Map qualified as M
import Data.Maybe (fromJust)

type Graph = M.Map String Node

type Label = String

data Node = Node {name :: Label, neighbours :: [Label]} deriving (Show)

readNode :: String -> Node
readNode xs =
  let n = take 3 xs
      ns = words $ drop 4 xs
   in Node {name = n, neighbours = ns}

dfs :: Label -> Label -> Graph -> [Label] -> Int
dfs current target graph visited = fst $ go current visited M.empty
  where
    go current visited cache
      | current == target = (1, cache)
      | current == "out" = (0, cache)
      | otherwise =
          let key = current
           in case M.lookup key cache of
                Just result -> (result, cache)
                Nothing ->
                  let node = fromJust $ M.lookup current graph
                      next = neighbours node
                      (result, cache') =
                        foldl
                          ( \(acc, c) n ->
                              let (r, c') = go n (current : visited) c
                               in (acc + r, c')
                          )
                          (0, cache)
                          next
                      cache'' = M.insert key result cache'
                   in (result, cache'')

part1 :: Graph -> Int
part1 graph = dfs "you" "out" graph []

part2 :: Graph -> Int
part2 graph =
  let svrToFft = dfs "svr" "fft" graph []
      fftToDac = dfs "fft" "dac" graph []
      dacToOut = dfs "dac" "out" graph []
      svrToDac = dfs "svr" "dac" graph []
      dacToFft = dfs "dac" "fft" graph []
      fftToOut = dfs "fft" "out" graph []
   in svrToDac * dacToFft * fftToOut + svrToFft * fftToDac * dacToOut

generateDotFile :: String -> [Node] -> IO ()
generateDotFile filename nodes =
  let pre = "digraph G {\n\nsvr[color=blue]\nout[color=orange]\nfft[color=green]\ndac[color=red]\n\n"
      edges = [(name n) ++ " -> " ++ (n') ++ ";" | n <- nodes, n' <- neighbours n]
      post = "\n}"
   in writeFile filename $ pre ++ (unlines edges) ++ post

main :: IO ()
main = do
  nodes <- (map readNode . lines) <$> readFile "./data/day11.txt"
  let graph = M.fromList [(name n, n) | n <- nodes]
  generateDotFile "day11.dot" nodes
  print $ part1 graph
  print $ part2 graph