import Data.Char (toLower)

-- Exercise 2

f1 :: [Double] -> [Int]
f1 xs = [floor x | x <- xs]

f2 :: [Double] -> [Int]
f2 [] = []
f2 (x : xs) = floor x : (f2 xs)

f3 :: [Double] -> [Int]
f3 = map floor

-- map :: (a -> b) -> [a] -> [b]
-- map floor :: [a] -> [b]

d11 :: Int -> Bool
d11 n = n `mod` 11 == 0

-- Note: mod, rem (remainder), div (integer division)

toLowerCase :: String -> String
toLowerCase xs = [toLower x | x <- xs]

toLowerCase' :: String -> String
toLowerCase' = map toLower

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- Exercise 4
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filterBy11 :: [Int] -> [Int]
filterBy11 = filter' d11

-- Exercise 5:
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)

swap' :: (a, b) -> (b, a)
swap' (x, y) = (y, x)

apply :: a -> (a -> b) -> b
apply x f = f x

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g x = g (f x)

compose' :: (a -> b) -> (b -> c) -> a -> c
compose' f g x = g $ f x

compose'' :: (a -> b) -> (b -> c) -> a -> c
compose'' f g = g . f

-- Exercise 6

-- f :: Eq a => a -> [a] -> Bool
-- elem, notElem

-- g :: (a -> b) -> [a] -> [b]
-- map, map then reverse

-- h :: Ord a => [a] -> [a]
-- sort, sort the reverse