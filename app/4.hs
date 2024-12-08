main = do
    content <- readFile "input/4"
    let matrix = map (map (: [])) $ words content
    let part1 =
            countXMAS $
                concat
                    [ getAllDirections matrix i j
                    | i <- [0 .. (length matrix - 1)]
                    , j <- [0 .. (length (head matrix) - 1)]
                    ]
    putStrLn ("Part 1: " ++ show part1)
    let part2 =
            sum
                [ (fromEnum . checkIsMAS) (getDiagonals matrix i j)
                | i <- [0 .. (length matrix - 1)]
                , j <- [0 .. (length (matrix !! i) - 1)]
                ]
    putStrLn ("Part 2: " ++ show part2)

----------------------------------------------Part 1----------------------------------------------

getAllDirections :: [[String]] -> Int -> Int -> [[String]]
getAllDirections matrix i j =
    [ getDirection matrix i j rowSign colSign
    | rowSign <- [-1, 0, 1]
    , colSign <- [-1, 0, 1]
    , (rowSign, colSign) /= (0, 0)
    ]

getDirection :: [[String]] -> Int -> Int -> Int -> Int -> [String]
getDirection matrix i j rowSign colSign
    | validMove = [matrix !! (i + k * rowSign) !! (j + k * colSign) | k <- [0 .. 3]]
    | otherwise = []
  where
    validMove =
        (i + 3 * rowSign >= 0 && i + 3 * rowSign < length matrix)
            && (j + 3 * colSign >= 0 && j + 3 * colSign < length (matrix !! i))

countXMAS :: [[String]] -> Int
countXMAS l = sum $ map (fromEnum . (== ["X", "M", "A", "S"])) l

----------------------------------------------Part 2----------------------------------------------

getDiagonals :: [[String]] -> Int -> Int -> [[String]]
getDiagonals matrix i j = diagonal : [diagonal2]
  where
    diagonal = if isValidDiagonal diagIndices then [matrix !! (i + k) !! (j + k) | k <- [-1, 0, 1]] else []
    diagonal2 = if isValidDiagonal diag2Indices then [matrix !! (i - k) !! (j + k) | k <- [-1, 0, 1]] else []

    isValidDiagonal = all (\(i', j') -> i' >= 0 && i' < length matrix && j' >= 0 && j' < length (head matrix))

    diagIndices = [(i + k, j + k) | k <- [-1, 0, 1]]
    diag2Indices = [(i - k, j + k) | k <- [-1, 0, 1]]

checkIsMAS :: [[String]] -> Bool
checkIsMAS = all (\e -> e == goal || reverse e == goal)
  where
    goal = ["M", "A", "S"]
