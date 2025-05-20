import Control.Applicative
import Control.Monad
import Data.Char

-- Input-output

main1 :: IO ()
main1 = putStrLn "hello world"

-- compile with "ghc io"
-- run "./io"

main2 = do
  putStrLn "Enter your name"
  name <- getLine
  putStrLn ("Hello " ++ name)

main3 = do
  x <- putStrLn "Enter your name"
  name <- getLine
  putStrLn ("Hello " ++ name)

main4 = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main5 = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main5

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main6 = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line

main7 = do
  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

main8 = do
  print True
  print 2
  print "haha"
  print 3.2
  print [3, 4, 3]

main9 = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a, b, c]

main10 = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

main11 = mapM print [1, 2, 3]

main12 = mapM_ print [1, 2, 3]

main13 = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l

main14 = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          color <- getLine
          return color
      )
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors

-- Functors
-- has one method: fmap :: (a -> b) -> f a -> f b

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor IO where
  fmap :: (a -> b) -> IO a -> IO b
  fmap f action = do
    result <- action
    return (f result)

-- Functor laws
-- fmap id = id
-- fmap (g . f) = fmap g . fmap f

-- Applicative functors

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

l1 = [(* 0), (+ 100), (^ 2)] <*> [1, 2, 3]

l2 = [(+), (*)] <*> [1, 2] <*> [3, 4]

-- Monoids

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

-- Monads

class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  m >> n = m >>= \_ -> n

-- >>= is called "bind"

-- monad laws:
-- return a >>= k  =  k a
-- m >>= return    =  m
-- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

-- nicer laws using fish operator
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
g >=> h = \x -> g x >>= h

-- return >=> g  =  g
-- g >=> return  =  g
-- (g >=> h) >=> k  =  g >=> (h >=> k)

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just x >>= k = k x

listOfTuples = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples' = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)