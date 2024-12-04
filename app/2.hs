----Part 1-----
main = do
    content <- readFile "input_2.txt"
    let output = foldr (((+) . checkList) . map read . words) 0 (lines content)
    print output

----Part 2-----
main2 = do
    content <- readFile "input_2.txt"
    let output = foldr (((+) . checkAllPossibilities) . allPossibilities . map read . words) 0 (lines content)
    print output

---Aux Funcs---
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

allPossibilities :: [a] -> [[a]]
allPossibilities xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

checkAllPossibilities :: [[Int]] -> Int
checkAllPossibilities l = min (sum (map checkList l)) 1
