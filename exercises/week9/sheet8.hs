import Nats

data SignInt = Plus Nat | Minus Nat deriving (Show)

instance Eq SignInt where
  (==) (Plus Z) (Minus Z) = True
  (==) (Minus Z) (Plus Z) = True
  (==) (Plus n) (Plus m) = n == m
  (==) (Minus n) (Minus m) = n == m
  (==) _ _ = False

addSignInt :: SignInt -> SignInt -> SignInt
addSignInt (Plus n) (Plus m) = Plus (n + m)
addSignInt (Minus n) (Minus m) = Minus (n + m)
addSignInt (Plus n) (Minus m)
  | n >= m = Plus (n - m)
  | otherwise = Minus (m - n)
addSignInt (Minus n) (Plus m)
  | m >= n = Plus (m - n)
  | otherwise = Minus (n - m)

multSignInt :: SignInt -> SignInt -> SignInt
multSignInt (Plus n) (Plus m) = Plus (m * n)
multSignInt (Plus n) (Minus m) = Minus (m * n)
multSignInt (Minus n) (Plus m) = Minus (m * n)
multSignInt (Minus n) (Minus m) = Plus (m * n)

instance Ord SignInt where
  compare (Plus n) (Plus m) = compare n m
  compare (Minus n) (Minus m) = compare m n
  compare (Minus n) (Plus m)
    | n == Z && m == Z = EQ
    | otherwise = LT
  compare (Plus n) (Minus m)
    | n == Z && m == Z = EQ
    | otherwise = GT

data DiffInt = Diff Nat Nat deriving (Show)

diffToInt :: DiffInt -> Integer
diffToInt (Diff n m) = toInteger n - toInteger m

instance Eq DiffInt where
  (==) (Diff a b) (Diff c d) = (a + d) == (b + c)

addDiffInt :: DiffInt -> DiffInt -> DiffInt
addDiffInt (Diff a b) (Diff c d) = Diff (a + c) (b + d)

multDiffInt :: DiffInt -> DiffInt -> DiffInt
multDiffInt (Diff a b) (Diff c d) = Diff (a * c + b * d) (a * d + b * c)

instance Ord DiffInt where
  compare (Diff a b) (Diff c d) = compare (a + d) (c + b)