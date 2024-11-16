grow :: [a] -> [a]
grow xs = go xs 2
    where
        go [] _ = []
        go (x:xs) 1 = x : x : go xs 2
        go (x:xs) n = x : go xs (n-1)