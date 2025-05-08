import Data.Complex
import Data.Ratio

-- Exercise 1

newton :: (Rational -> Rational) -> (Rational -> Rational) -> Rational -> Int -> Rational
newton f f' x0 0 = x0
newton f f' x0 n = x - (f x / f' x)
  where
    x = newton f f' x0 (n - 1)

newton2 :: (Rational -> Rational) -> (Rational -> Rational) -> Rational -> Rational -> Rational
newton2 f f' x0 eps
  | abs (x0 - x1) < eps = x1
  | otherwise = newton2 f f' x1 eps
  where
    x1 = x0 - f x0 / f' x0

-- Use this to approximate sqrt 2 (newton f f' 2 10)

f :: (Num a) => a -> a
f x = x ^ 2 - 2

f' :: (Num a) => a -> a
f' x = 2 * x

-- Exercise 2

mandeliter :: Complex Double -> Int -> Complex Double
mandeliter c 0 = 0
mandeliter c n = z ^ 2 + c
  where
    z = mandeliter c (n - 1)

mandelbrot :: Complex Double -> Bool
mandelbrot c = magnitude (mandeliter c 10000) < 10 ^ 100

-- Using corecursion
ones = 1 : ones

nats = 0 : map (+ 1) nats

mandelseries :: (Num t) => t -> [t]
mandelseries c = 0 : map (\z -> z ^ 2 + c) (mandelseries c)

mandelbrot2 :: Complex Double -> Bool
mandelbrot2 c = magnitude (mandelseries c !! 1000) < 10 ^ 100