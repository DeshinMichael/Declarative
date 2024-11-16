pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (y:ys) = (y : takeWhile (== y) ys) : pack (dropWhile (== y) ys)