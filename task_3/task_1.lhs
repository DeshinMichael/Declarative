> integrate :: (Double -> Double) -> Double -> Double -> Double
> integrate f a b = integrate' f a b 1000 where
>   integrate' f a b 0 = 0
>   integrate' f a b n = (f (a) + f (a + h)) / 2 * h + integrate' f (a + h) b (n - 1)
>       where h = (b - a) / n

> integral1 = integrate (\x -> 2 * x ^ 2 + 3 * x - 1) (-3) 1
> integral2 = integrate ((2 *) . (^ 2)) 1 2
> integral3 = integrate ((1 /) . (^ 2) . cos) (-pi/4) 0
> integral4 = integrate (exp . negate . (^ 2)) (-1000) 1000
> integral5 = integrate (\t -> if t == 0 then 1 else sin t / t) 1e-10 1