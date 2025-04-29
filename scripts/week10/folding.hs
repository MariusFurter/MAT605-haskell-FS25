foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

-- (Imperative pseudocode)
-- val = z
-- for x in xs:
--  val = f(x, val)

maximum' :: [Int] -> Int
maximum' xs = foldr' max (minBound :: Int) xs

and' :: [Bool] -> Bool
and' = foldr' (&&) True

or' :: [Bool] -> Bool
or' = foldr' (||) True

sum' :: (Num a) => [a] -> a
sum' = foldr' (+) 0

product' :: (Num a) => [a] -> a
product' = foldr' (*) 1

length' :: [a] -> Int
length' = foldr' (\x val -> val + 1) 0

-- f :: a -> Int -> Int
-- f = \x val -> val + 1

elem' :: (Eq a) => a -> [a] -> Bool
elem' s = foldr' (\x val -> val || x == s) False

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x filtered -> if p x then x : filtered else filtered) []

reverse' :: [a] -> [a]
reverse' = foldr' (\x reversed -> reversed ++ [x]) []

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x : xs) = foldl' f (f z x) xs

reverse'' :: [a] -> [a]
reverse'' = foldl' (\reversed x -> x : reversed) []