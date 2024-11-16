> toSeries :: String -> [String]
> toSeries = foldr (\x acc -> if null acc || x /= head (head acc) then [x] : acc else (x : head acc) : tail acc) []