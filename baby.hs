import Data.Char (toLower, isSpace)

isPalindrome2 :: String -> Bool
isPalindrome2 xs = processed == reverse processed
  where
    processed = map toLower (filter (not . isSpace) xs)

replaceAt :: a -> [a] -> Int -> [a]
replaceAt v xs i
  | i < 0 || i >= length xs = error "index out of range"
  | otherwise = take i xs ++ [v] ++ drop (i + 1) xs

dropEveryNth :: [a] -> Int -> [a]
dropEveryNth xs n | n <= 0 = error "n should be greater than 0" 
dropEveryNth xs n = go xs n
  where
    go [] _ = []
    go (y:ys) 1 = go ys n 
    go (y:ys) m = y : go ys (m - 1)

f :: Double -> Double
f x = (sqrt (x + 13) - 2 * sqrt (x + 1)) / (x ^ 2 - 9)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = zip [0..] xs
onlyEvenElements :: [a] -> [a]
onlyEvenElements xs = map snd (filter (\(x, _) -> even x) (zipWithIndex xs))

triple :: Eq a => [a] -> [a]
triple xs = go xs
  where
    go [] = []
    go (y:ys) = y : y : y : go ys

squeeze :: Eq a => [a] -> [a]
squeeze [] = []
squeeze [x] = [x]
squeeze (x:y:xs)
  | x == y = squeeze (y:xs)
  | otherwise = x : squeeze (y:xs)



allGrades :: [GradeBook]

GradeBook {
  student :: Student
  grades :: [SubjectGrade]
}

Student {
  firstName :: String
  lastName :: String
}

SubjectGrade {
  nameOfSubject :: String
  grades :: [Int]
}

-- 1. Закодировать типы данных на Haskell, с помощью ключевого слова data
-- 2. Реализовать форматированную печать для каждого типа данных (instance Show ...)
-- 3. Создать экземпляр allGrades, протестировать работу функций