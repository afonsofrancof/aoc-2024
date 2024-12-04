main = do
    content <- readFile "app/input_4.txt"
    let matrix = map (map (: [])) $ words content
    let result =
            concat
                [ getAllDirections matrix i j
                | i <- [0 .. (length matrix - 1)]
                , j <- [0 .. (length (head matrix) - 1)]
                ]
    print $ countXMAS result

getDirection :: [[String]] -> Int -> Int -> Int -> Int -> [String]
getDirection matrix i j rowSign colSign
    | validMove = [matrix !! (i + k * rowSign) !! (j + k * colSign) | k <- [0 .. 3]]
    | otherwise = []
  where
    validMove =
        (i + 3 * rowSign >= 0 && i + 3 * rowSign < length matrix)
            && (j + 3 * colSign >= 0 && j + 3 * colSign < length (matrix !! i))

getAllDirections :: [[String]] -> Int -> Int -> [[String]]
getAllDirections matrix i j =
    [ getDirection matrix i j rowSign colSign
    | rowSign <- [-1, 0, 1]
    , colSign <- [-1, 0, 1]
    , (rowSign, colSign) /= (0, 0)
    ]

countXMAS :: [[String]] -> Int
countXMAS l = sum $ map (fromEnum . (== ["X", "M", "A", "S"])) l

----------------------------------------------2-------------------------------------------

main2 = do
    content <- readFile "app/input_4.txt"
    let matrix = map (map (: [])) $ words content
    let result =
            [ getDirections matrix i j
            | i <- [0 .. (length matrix - 1)]
            , j <- [0 .. (length (matrix !! i) - 1)]
            ]
    print $ sum (map (fromEnum . checkIsMAS) result)

getDirections :: [[String]] -> Int -> Int -> [[String]]
getDirections matrix i j = diag : [diag2]
  where
    diag = if validDiag diagIndices then [matrix !! (i + k) !! (j + k) | k <- [-1, 0, 1]] else []
    diag2 = if validDiag diag2Indices then [matrix !! (i - k) !! (j + k) | k <- [-1, 0, 1]] else []

    validDiag = all (\(i', j') -> i' >= 0 && i' < length matrix && j' >= 0 && j' < length (head matrix))

    diagIndices = [(i + k, j + k) | k <- [-1, 0, 1]]
    diag2Indices = [(i - k, j + k) | k <- [-1, 0, 1]]

checkIsMAS :: [[String]] -> Bool
checkIsMAS = all (\e -> e == ["M", "A", "S"] || e == ["S", "A", "M"])
