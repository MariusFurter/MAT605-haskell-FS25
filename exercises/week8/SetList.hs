module SetList where

import Data.List (nub)

-- Datatype for Sets
data Set a = Set [a] deriving (Show)

instance (Eq a) => Eq (Set a) where
  (==) :: (Eq a) => Set a -> Set a -> Bool
  s == t = s `subSet` t && t `subSet` s

unSet :: Set a -> [a]
unSet (Set xs) = xs

toSet :: (Eq a) => [a] -> Set a
toSet xs = Set (nub xs)

inSet :: (Eq a) => a -> Set a -> Bool
inSet x s = x `elem` unSet s

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x : xs)) t = x `inSet` t && Set xs `subSet` t

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet s t = toSet (unSet s ++ unSet t)

intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet s t = toSet [x | x <- unSet s, x `inSet` t]