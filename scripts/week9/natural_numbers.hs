data Nat = Z | S Nat deriving (Show)

-- Conversion
fromNat :: Nat -> Integer
fromNat Z = 0
fromNat (S n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Z
toNat n
  | n > 0 = S (toNat (n - 1))
  | otherwise = error "number is negative"

-- Arithmetic
plus :: Nat -> Nat -> Nat
plus n Z = n
plus n (S m) = S (n `plus` m)

plus' :: Nat -> Nat -> Nat
plus' Z m = m
plus' (S n) m = S (n `plus` m)

mult :: Nat -> Nat -> Nat
mult n Z = Z
mult n (S m) = plus (mult n m) n

pow :: Nat -> Nat -> Nat
pow n Z = S Z
pow n (S m) = mult n (pow n m)

-- Comparison
leq :: Nat -> Nat -> Bool
leq Z m = True
leq (S n) Z = False
leq (S n) (S m) = leq n m

geq :: Nat -> Nat -> Bool
geq n m = leq m n

lt :: Nat -> Nat -> Bool
lt n m = not (geq n m)

gt :: Nat -> Nat -> Bool
gt n m = not (leq n m)

-- Folds over Nats
-- Apply a function h :: a -> a N times to a value c :: a.
-- foldn h c N = h ( h ( h ... (h (c))))
foldn :: (a -> a) -> a -> Nat -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

foldplus :: Nat -> Nat -> Nat
foldplus = foldn S

---
foldmult :: Nat -> Nat -> Nat
foldmult n = foldn (foldplus n) Z

foldpow :: Nat -> Nat -> Nat
foldpow n = foldn (foldmult n) (S Z)

foldwow :: Nat -> Nat -> Nat
foldwow n = foldn (foldpow n) n
