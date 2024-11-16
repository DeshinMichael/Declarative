> makeAssoc :: Eq a => [(a, b)] -> a -> b
> makeAssoc xs = \key -> foldr (\(k, v) acc -> if key == k then v else acc) (error "no such key") xs