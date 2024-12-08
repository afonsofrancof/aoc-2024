import Text.Regex.TDFA

data Instruction
    = Mul Int Int -- mul(a, b)
    | Do -- do()
    | Dont -- don't()
    deriving (Show)

main = do
    input <- readFile "input/3"
    let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
        part1 = sum $ map product $ getMatches input pattern
    putStrLn ("Part 1: " ++ show part1)
    let pattern2 = "(mul\\(([0-9]{1,3}),([0-9]{1,3})\\))|do\\(\\)|don't\\(\\)"
        part2 = sum $ map product $ getMatches2 input pattern2
    putStrLn ("Part 2: " ++ show part2)

----------------------------------------------Part 1----------------------------------------------

getMatches :: String -> String -> [[Int]]
getMatches input pattern = map (map read . tail) (input =~ pattern :: [[String]])

----------------------------------------------Part 2----------------------------------------------

getMatches2 :: String -> String -> [[Int]]
getMatches2 input pattern =
    map
        (map read . tail . tail)
        (filterList (input =~ pattern :: [[String]]) False)

filterList :: [[String]] -> Bool -> [[String]]
filterList [] _ = []
filterList (h : t) False
    | head h == "don't()" = filterList t True
    | head h == "do()" = filterList t False
    | otherwise = h : filterList t False
filterList (h : t) True
    | head h == "don't()" = filterList t True
    | head h == "do()" = filterList t False
    | otherwise = filterList t True
