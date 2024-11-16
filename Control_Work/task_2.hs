depolyndromize :: [String] -> [String]
depolyndromize xs
    | even (length xs) = take ((length xs) `div` 2) xs
    | otherwise = take ((length xs) `div` 2 + 1) xs 