> a_zip :: [a] -> [b] -> [(a, b)]
> a_zip (k:ks) (v:vs) = (k, v) : a_zip ks vs
> a_zip _ _ = []