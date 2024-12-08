import Data.Graph
import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

type PairMap = Map.Map Int [Int]

createMap :: [[Int]] -> PairMap
createMap = Map.fromListWith (++) . map (\[k, v] -> (k, [v]))

main :: IO ()
main = do
    content <- readFile "input/5"
    -- Get list of pairs
    let [pairsPart, listsPart] = splitOn "\n\n" content
        pairMap = createMap . map (map read . splitOn "|") $ lines pairsPart
        lists = map (map read . splitOn ",") $ lines listsPart

    -- Get and sum middle of correct pages
    let correctUpdates = [xs | xs <- lists, not (anyMatch [] xs pairMap)]
        part1Sum = sum $ map middle correctUpdates
    putStrLn ("Part 1: " ++ show part1Sum)

    -- Fix and sum middle of incorrect pages
    let incorrectUpdates = [xs | xs <- lists, anyMatch [] xs pairMap]
        fixedUpdates = map (topologicalSort pairMap) incorrectUpdates
        part2Sum = sum $ map middle fixedUpdates
    putStrLn ("Part 2: " ++ show part2Sum)


----------------------------------------------Part 1----------------------------------------------

middle :: [Int] -> Int
middle l = l !! (length l `div` 2)

anyMatch :: [Int] -> [Int] -> PairMap -> Bool
anyMatch visited [] _ = False
anyMatch visited (h : t) pairMap =
    any (`elem` visited) (Map.findWithDefault [] h pairMap) || anyMatch (h : visited) t pairMap

----------------------------------------------Part 2----------------------------------------------

topologicalSort :: PairMap -> [Int] -> [Int]
topologicalSort pairMap nodes =
    -- Here we build the graph, for each element of the list.
    -- If an element doesn't exist in the pairMap, we add it to the graph paired with an empty list
    let (graph, vertexToNode, vertexFromKey) = graphFromEdges [(n, n, Map.findWithDefault [] n pairMap) | n <- nodes]
    -- Here we perform a topologicalSort of the map and get the edges in order
     in map (getTuple1st . vertexToNode) $ topSort graph

getTuple1st (a,_,_) = a
