-- Defining Custom Types --

-- Enumeration types:
data Thing = Thing1 | Thing2 | Thing3 deriving (Show)

-- The `Thing` on the left is called 'type constructor'.
-- `Thing1` is called 'data constructor'.
-- `deriving Show` automatically makes the new type printable.

-- Enumerations can be pattern matched by cases:
whichThing :: Thing -> Int
whichThing Thing1 = 1
whichThing Thing2 = 2
whichThing Thing3 = 3

-- Types can have fields:
data Person = Person String String deriving (Show)

p1 :: Person
p1 = Person "Marius" "Furter"

-- Fields can be pattern matched as follows:
firstName :: Person -> String
firstName (Person x y) = x

-- You can mix and match enumeration and fields
data FailOrDouble = Fail | OK Double deriving (Show)

f1 :: FailOrDouble
f1 = Fail

f2 :: FailOrDouble
f2 = OK 4.0

safeDivide :: Double -> Double -> FailOrDouble
safeDivide x y
  | y == 0 = Fail
  | otherwise = OK (x / y)

-- Types can be recursive:
data IntList = Empty | Cons Int IntList

l :: IntList
l = Cons 3 (Cons 4 Empty)

l' :: IntList
l' = Cons 3 $ Cons 4 Empty

intLength :: IntList -> Int
intLength Empty = 0
intLength (Cons x xs) = 1 + intLength xs

-- Datatypes can be polymorphic (depend on other types)
data Pair a b = Pair a b

p :: Pair Char Int
p = Pair 'a' 5

q :: Pair Double String
q = Pair 5.0 "Hello"

data List t = E | C t (List t)

i :: List Int
i = C 3 (C 5 E)

i' :: List Char
i' = C 'a' (C 'b' E)

mapList :: (t1 -> t2) -> List t1 -> List t2
mapList f (C x xs) = C (f x) (mapList f xs)

-- Typeclasses --
-- Typeclasses define common functions that each member must implement.
class Intable a where
  toInt :: a -> Int

-- We can declare a type an instance by implementing all the required functions.
instance Intable Thing where
  toInt :: Thing -> Int
  toInt = whichThing

-- Reimplementing the Eq typeclass:

class Eq' a where
  eq :: a -> a -> Bool
  neq :: a -> a -> Bool
  a `neq` b = not (a `eq` b)
  a `eq` b = not (a `neq` b)

instance Eq' Thing where
  Thing1 `eq` Thing1 = True
  Thing2 `eq` Thing2 = True
  Thing3 `eq` Thing3 = True
  _ `eq` _ = False

instance Eq' Person where
  Person x y `eq` Person u v = (x == u) && (y == v)

instance (Eq a, Eq b) => Eq' (Pair a b) where
  Pair x y `eq` Pair u v = (x == u) && (y == v)

-- Equality and order can be derived (deriving (Eq, Ord))