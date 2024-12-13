> safeLog :: Double -> Maybe Double
> safeLog x
>   | x > 0 = Just (log x)
>   | otherwise = Nothing

> safeTg :: Double -> Maybe Double
> safeTg x
>   | cos x /= 0 = Just (tan x)
>   | otherwise  = Nothing

> safeCos :: Double -> Maybe Double
> safeCos x
>   | cos x /= 0 = Just (cos x)
>   | otherwise  = Nothing

> safeDiv :: Double -> Double -> Maybe Double
> safeDiv _ 0 = Nothing
> safeDiv a b = Just (a / b)

> fsafe :: Double -> Maybe Double
> fsafe x = do
>   lnEx <- safeLog (exp x)
>   tgLnEx <- safeTg lnEx
>   cosX <- safeCos x
>   term2 <- safeDiv 1 (2 * cosX)
>   let numerator = tgLnEx + term2
>   let denominator = x^2 + 2 * x - 10
>   safeDiv numerator denominator