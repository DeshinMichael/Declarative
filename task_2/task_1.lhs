Функция для вычисления арктангенса 
с помощью ряда Тейлора с заданной точностью eps > 0

> atan' :: Double -> Double -> Double -> Double -> Int -> Double
> atan' x eps sum term n
>   | abs (newSum - sum) < eps = sum
>   | otherwise = atan' x eps newSum newTerm (n + 1)
>   where
>       term = if even n
>                then x ** fromIntegral (2*n + 1) / fromIntegral (2*n + 1)
>                else - x ** fromIntegral (2*n + 1) / fromIntegral (2*n + 1)
>       newSum = sum + term
>       newTerm = term