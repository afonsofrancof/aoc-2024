import Data.List (sort, transpose)

-- From the 'containers' library
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set

main = do
    content <- readFile "input/1"
    let linesOfFile = lines content
        numbers = map (map read . words) linesOfFile :: [[Int]]
        [firstColumn, secondColumn] = transpose numbers
    let part1 = ex1 $ zip (sort firstColumn) (sort secondColumn)
    putStrLn ("Part 1: " ++ show part1)
    let part2 = ex1_2 firstColumn (consolidatePair secondColumn)
    putStrLn ("Part 2: " ++ show part2)

----------------------------------------------Part 1----------------------------------------------

ex1 [] = 0
ex1 ((h, h1) : t) = abs (h - h1) + ex1 t

----------------------------------------------Part 2----------------------------------------------

consolidatePair :: (Ord a, Num b) => [a] -> Map.Map a b
consolidatePair lst = Map.fromListWith (+) (map (,1) lst)

ex1_2 :: [Int] -> Map.Map Int Int -> Int
ex1_2 [] _ = 0
ex1_2 (h : t) secondColumn = case Map.lookup h secondColumn of
    Just hl -> hl * h + ex1_2 t secondColumn
    Nothing -> ex1_2 t secondColumn
