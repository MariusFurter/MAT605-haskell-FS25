-- Types
doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> (Int -> Int)
doubleUs x y = 2 * (x + y)

apply :: Int -> (Int -> Int) -> Int
apply x f = f x

-- Type variables

fst' :: (a, b) -> a
fst' (x, y) = x

-- Typeclasses

doubleMe' :: (Num a) => a -> a
doubleMe' x = x + x

-- Eq (==)
-- Ord (<=, <, >, =>)
-- Num,
-- Show, Read,
-- Integral, Floating, Fractional

isEqual :: (Eq a) => a -> a -> Bool
isEqual x y = x == y

-- fromIntegral
l = [1, 2, 3]

len = fromIntegral (length l) / 4.5

-- Pattern matching

sayNumber :: Int -> String
sayNumber x =
  if x == 1
    then "This is one"
    else "This is a number"

sayNumber' :: Int -> String
sayNumber' 1 = "This is one"
sayNumber' 2 = "This is two"
sayNumber' _ = "This is a number"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors x y = (fst x + fst y, snd x + snd y)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x_1, x_2) (y_1, y_2) = (x_1 + y_1, x_2 + y_2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (x, y, z) = y

head' :: [a] -> a
head' [] = error "Empty list!"
head' (x : xs) = x

unCons :: [a] -> (a, [a])
unCons (x : xs) = (x, xs)

-- Exercise: Write a function that calculates scalar product of vectors

sayLength :: [a] -> String
sayLength [] = "Zero"
sayLength [x] = "One"
sayLength [x, y] = "Two"
sayLength (x : xs) = "Longer than Two"

sayLength' :: [a] -> String
sayLength' [] = "Zero"
sayLength' (x : []) = "One"
sayLength' (x : y : []) = "Two"
sayLength' (x : y : _) = "More than two"

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = length' xs + 1

length'' :: [a] -> Int
length'' [] = 0
length'' xs = length'' (tail xs) + 1

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- In future:
sum'' :: (Foldable t, Num b) => t b -> b
sum'' = foldr (+) 0

firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter s@(x : xs) = "The first letter of " ++ s ++ " is " ++ [x]

-- Guards

grade :: Double -> String
grade p
  | p <= 5 = "Fail"
  | p <= 10 = "C"
  | p <= 15 = "B"
  | otherwise = "A"

abs' :: (Ord a, Num a) => a -> a
abs' x
  | x >= 0 = x
  | x < 0 = -x

-- Where

dist :: (Ord a, Num a) => a -> a -> a
dist x y
  | diff >= zero = diff
  | diff < zero = -diff
  where
    diff = x - y
    zero = 0

dist' :: (Ord a, Num a) => a -> a -> a
dist' x y = abs' diff
  where
    diff = x - y

cylinder :: Double -> Double -> Double
cylinder r h = 2 * topArea + sideArea
  where
    topArea = pi * r ^ 2
    sideArea = h * 2 * pi * r

-- Let
cylinder' :: Double -> Double -> Double
cylinder' r h =
  let topArea = pi * r ^ 2
      sideArea = h * 2 * pi * r
   in 2 * topArea + sideArea

-- Case (pattern matching inside function)
head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "Empty list!"
  (x : xs) -> x
