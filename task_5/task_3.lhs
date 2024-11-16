> writeCode :: [(Int, Char)] -> String
> writeCode = foldr (\x acc -> show (fst x) ++ [snd x] ++ acc) ""