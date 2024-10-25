Функция для поиска значения по ключу

> a_find :: Eq a => [(a, b)] -> a -> b
> a_find [] _ = error "no such key"
> a_find (kv:kvs) a
>   | fst kv == a = snd kv
>   | otherwise = a_find kvs a