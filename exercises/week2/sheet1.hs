import Data.Char (toLower)

-- Exercise 2
f1 :: [Double] -> [Int]
f1 xs = [floor x | x <- xs]

d11 :: Int -> Bool
d11 x = x `mod` 11 == 0

toLowerCase :: String -> String
toLowerCase s = [toLower ch | ch <- s]

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- Exercise 4
filter' :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- Exercise 5
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)

apply :: a -> (a -> b) -> b
apply a f = f a

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x)

-- Exercise 6
-- f: elem, notElem, allEqual
-- g: map, map then reverse
-- h: sort, sort then reverse
