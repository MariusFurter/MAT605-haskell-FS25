-- Exercise 1
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

-- Exercise 2
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- Exercise 3
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

scalarProd :: (Num a) => [a] -> [a] -> a
scalarProd xs ys = sum $ zipWith' (*) xs ys

-- Exercise 4
flatten' :: [[a]] -> [a]
flatten' [] = []
flatten' (x : xs) = x ++ flatten' xs

-- Exercise 5
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [2 * x]
doubleEveryOther (x : y : xs) = 2 * x : y : doubleEveryOther xs

-- Exercise 6
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits firstDigits ++ [lastDigit]
  where
    lastDigit = n `mod` 10
    firstDigits = n `div` 10