Функция в качестве примера
> f :: Double -> Double
> f x = 2 ** x - x ** 2

Функция для поиска корня
> solver :: (Double -> Double) -> Double -> Double -> Double -> Double
> solver f a b eps
>   | f a * f b > 0 = error "Одинаковые знаки"
>   | otherwise = alg f a b eps
>   where
>      alg f a b eps = 
>       let c = (a + b) / 2
>       in if abs (f c) < eps
>           then c
>           else if signum (f a) == signum (f c)
>               then solver f c b eps
>               else solver f a c eps