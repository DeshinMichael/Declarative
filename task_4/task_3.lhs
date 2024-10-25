Функция для поиска уникальных ключей

> a_domain :: Eq a => [(a, b)] -> [a]
> a_domain abs = deleteDubls [a | (a, _) <- abs] -- Функция для удаления дубликатов
>   where
>       deleteDubls [] = []
>       deleteDubls (x:xs)
>           | x `elem` xs = x : deleteDubls (filter (/= x) xs)
>           | otherwise = x : deleteDubls xs