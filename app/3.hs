import Text.Regex.TDFA

data Instruction
    = Mul Int Int -- mul(a, b)
    | Do -- do()
    | Dont -- don't()
    deriving (Show)

main = do
    input <- readFile "app/input_3.txt"
    let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
        result = sum $ map product $ getMatches input pattern
        pattern2 = "(mul\\(([0-9]{1,3}),([0-9]{1,3})\\))|do\\(\\)|don't\\(\\)"
        result2 = sum $ map product $ getMatches2 input pattern2
    print result
    print result2

getMatches :: String -> String -> [[Int]]
getMatches input pattern = map (map read . tail) (input =~ pattern :: [[String]])

getMatches2 :: String -> String -> [[Int]]
getMatches2 input pattern =  map (map read . tail . tail) (filterList (input =~ pattern :: [[String]]) False
        )
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
