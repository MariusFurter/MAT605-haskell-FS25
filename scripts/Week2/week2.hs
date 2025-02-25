-- Chapter 2 (learnyouahaskell.com)

-- Arithmetic + - * / ^
-- Logical Operations (True / False) &&, ||, not
-- Comparison: ==, /=, <, <=, >, >=
-- Convert infix f to prefix by (f)
-- Convert prefix function f to infix `f`

-- Basic Function
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else 2 * x

seven = "seven"

-- Lists [1,2,3]
-- Concatenation ++
-- Cons: : (every list [1,2,3] == 1 : 2 : 3 : [] )
-- Get index: !! (indexing starts at 0)
-- head, tail, last, init
-- maximum, minimum, sum, product
-- reverse
-- elem

-- Ranges [1..10] / [1,4..12] / [1..] (Lazy!)

-- Comprehension 
-- [2*x | x <- [1,2,3]]
-- [x | x <- [1..10], even x]
-- [(x,y) | x <- [1..3], y <- [1..3]]

isVowel x = x `elem` ['a', 'e', 'u', 'o', 'i', 'y']

removeVowels xs = [letter | letter <- xs, not (isVowel letter)]

-- Exercise Create a list of triples (a,b,c) where a^2 + b^2 = c^2
pythagorean a b c = a ^ 2 + b ^ 2 == c ^ 2

pyTripples = [(a, b, c)
             | a <- [0 .. 10]
             , b <- [0 .. 10]
             , c <- [0 .. 10]
             , pythagorean a b c]

-- Tuples (1,3,4) / (1,'a')
-- fst, snd
-- zip

l = zip [1 ..] ['a' .. 'h']

-- Chapter 2 (learnyouahaskell.com) Types / Typeclasses

doubleMe' :: Int -> Int
doubleMe' x = x + x

doubleMe'' :: Float -> Float
doubleMe'' x = x + x

doubleMe''' :: Num a => a -> a
doubleMe''' x = x + x

addUs :: Int -> Int -> Int
addUs x y = x + y

-- Some basic types (use :t to check types)
-- Numbers: Int, Integer, Float, Double
-- Bool (True / False)
-- Char ('a')
-- String ("hello" = ['h','e', 'l', 'l', 'o']) 
-- List ([1,2,3] :: [Int])
-- Tuples ((1,'a') :: (Int, Char) )
-- Function types: ( a -> b )