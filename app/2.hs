main = do
    content <- readFile "input/2"
    let part1 = foldr (((+) . checkList) . map read . words) 0 (lines content)
    putStrLn ("Part 1: " ++ show part1)
    let part2 = foldr (((+) . checkAllPossibilities) . allPossibilities . map read . words) 0 (lines content)
    putStrLn ("Part 2: " ++ show part2)

----------------------------------------------Part 1----------------------------------------------

checkList l = if checkDists l && isStrict l then 1 else 0
  where
    checkDists :: [Int] -> Bool
    checkDists l = maximum op <= 3 && minimum op >= 1
      where
        op = zipWith (\a b -> abs (a - b)) l (tail l)

    isStrict :: [Int] -> Bool
    isStrict xs = and (pairs (<)) || and (pairs (>))
      where
        pairs op = zipWith op xs (tail xs)

----------------------------------------------Part 2----------------------------------------------

allPossibilities :: [a] -> [[a]]
allPossibilities xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

checkAllPossibilities :: [[Int]] -> Int
checkAllPossibilities l = min (sum (map checkList l)) 1
