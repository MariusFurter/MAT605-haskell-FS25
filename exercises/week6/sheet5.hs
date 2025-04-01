import Data.List (nub)

data Set a = Set [a] deriving (Show)

instance (Eq a) => Eq (Set a) where
  (==) :: Set a -> Set a -> Bool
  s == t = (s `subSet` t) && (t `subSet` s)

-- Exercise 1
-- (i) Set ['a','b','c'] :: Set Char
-- (ii) Duplicates are retained
-- (iii) Error: all elements need be of the same type

card :: Set a -> Int
card (Set xs) = length xs

-- Exercise 2
unSet :: Set a -> [a]
unSet (Set xs) = xs

toSet :: (Eq a) => [a] -> Set a
toSet xs = Set (nub xs)

-- Exercise 3
inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set xs) = x `elem` xs

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x : xs)) t = x `inSet` t && Set xs `subSet` t

subSet' :: (Eq a) => Set a -> Set a -> Bool
subSet' (Set xs) (Set ys) = and [x `elem` ys | x <- xs]

subSet'' :: (Eq a) => Set a -> Set a -> Bool
subSet'' s t = all (`inSet` t) (unSet s)

-- Aside: Anonymous functions
add1 = \x -> x + 1

-- (`inset` t ) is equivalent to \x -> x `inSet` t

-- Exercise 4
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = toSet (xs ++ ys)

intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = toSet [x | x <- xs, x `elem` ys]

intersectSet' :: (Eq a) => Set a -> Set a -> Set a
intersectSet' (Set xs) (Set ys) = toSet $ filter (`elem` ys) xs
