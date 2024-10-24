Функция для вычисления суммы ряда геометрической прогрессии
через рекурсию с заданной точностью eps > 0

> geomSumEps :: Double -> Double -> Double -> Double
> geomSumEps b q eps 
>  | abs q > 1 || q == 1 || eps <= 0 = error "Не выполнено условие"
>  | abs (b/q - b) < eps = b
>  | otherwise = b + geomSumEps (b * q) q eps