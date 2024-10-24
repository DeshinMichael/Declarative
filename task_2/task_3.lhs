Функция вычисления n-го числа Фибоначчи с использованием хвостовой рекурсии

> fib :: Integer -> Integer
> fib n = fibTail n 1 1
>  where
>    fibTail 0 a _ = a
>    fibTail n a b = fibTail (n - 1) b (a + b)