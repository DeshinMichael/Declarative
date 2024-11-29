> import Control.Applicative (liftA2)

> primesUpToN :: Int -> [Int]
> primesUpToN n = filter isPrime [2..n]
>   where
>       isPrime x = not (x `elem` composites)
>       composites = [a * b | (a, b) <- liftA2 (,) [2..n] [2..n], a * b <= n]