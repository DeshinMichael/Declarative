s n = (n^2 - 1) / (2*n^2 - n - 1)

lim''' eps = helper 0 
  where 
    helper n 
      | estimation < eps = nth 
      | otherwise = helper (n + 1) 
      where 
        nth = s n 
        estimation = (nth - (1 / 2))

sumOfFirst10 xs = sum (take 10 xs)