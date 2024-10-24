Функция для вычисления суммы ряда геометрической прогрессии
через рекурсию 

> sumGeom :: Double -> Double -> Int -> Double
> sumGeom b0 q n
>  | n == 1 = b0
>  | otherwise = b0 + sumGeom (b0 * q) q (n - 1)