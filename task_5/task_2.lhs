> encodeSeries :: [String] -> [(Int, Char)]
> encodeSeries = foldl (\acc x -> acc ++ [(length x, head x)]) []