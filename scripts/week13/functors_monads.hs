import Control.Applicative
import Control.Monad

-- Functors
-- has a method to lift functions

-- Functor laws:
-- fmap id = id
-- fmap (g . f) = fmap g . fmap f

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

instance Functor [] where
  fmap = map

instance IO where
  fmap f action = do
    result <- action
    return (f result)

-- Applicative functors
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> (f a -> f b)

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

-- produce all possible sums and products of [1,2] and [3,4]
l1 = [(+), (*)] <*> [1, 2] <*> [3, 4]

-- Monads
class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  m >> n = m >>= \x -> n

-- >>= is called "bind"

-- monad laws
-- return x >>= k = k x
-- y >>= return = y
-- y >>= (\x -> k x >== h) = (y >>= k) >>= h

-- nicer laws using fish operator
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
g >=> h = \x -> g x >>= h

instance Monad [] where
  return x = [x]
  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concat $ map f xs

sqrt' :: Double -> [Double]
sqrt' 0 = [o]
sqrt' x = [-sqrt x, sqrt x]

sqrts = [0, 1, 2, 3] >>= sqrt'

instance Monad Maybe where
  return x = Just x
  Nothing >>= _ = Nothing
  (Just x) >>= k = k x

-- do construct for monads

listOfTuples = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples' = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)