-- Defining Custom Types --

-- Enumeration Types:
data Thing = Thing1 | Thing2 | Thing3 deriving (Show)

-- The `Thing` on the left is called 'type constructor'
-- `Thing1` ect are called 'data constructors'

-- Enumerations can be pattern matched using cases:
whichThing :: Thing -> Int
whichThing Thing1 = 1
whichThing Thing2 = 2
whichThing Thing3 = 3

-- Type can have fields
data Person = Person String String deriving (Show)

p1 :: Person
p1 = Person "FirstName" "LastName"

-- Fields can be pattern matched as follows:
firstName :: Person -> String
firstName (Person x y) = x

-- You can mix enumerations and fields
data FailOrDouble = Fail | OK Double deriving (Show)

f1 :: FailOrDouble
f1 = Fail

f2 :: FailOrDouble
f2 = OK 5.0

safeDivide :: Double -> Double -> FailOrDouble
safeDivide x y
  | y == 0 = Fail
  | otherwise = OK (x / y)

-- Types can be recursive
data IntList = Empty | Cons Int IntList deriving (Show)

l :: IntList
l = Cons 2 (Cons 3 (Cons 4 Empty))

l' :: IntList
l' = Cons 2 $ Cons 3 $ Cons 4 Empty

intLength :: IntList -> Int
intLength Empty = 0
intLength (Cons x xs) = 1 + intLength xs

-- Datatypes can be polymorphic
data Pair a b = Pair a b deriving (Show)

p :: Pair Double Double
p = Pair 1.0 2.0

q :: Pair Char Double
q = Pair 'x' 2.0

data List t = E | C t (List t) deriving (Show)

i :: List Int
i = C 3 (C 5 E)

ii :: List Char
ii = C 'a' (C 'b' E)

-- Record syntax (look it up if you want)
-- Can be used to define types with fields in a ways that automatically creates all projection functions.

-- Typeclasses --
-- Typeclasses define common functions for their members

class Intable a where
  toInt :: a -> Int

-- We can declare an instance of this typeclass by implementing all the required functions
instance Intable Thing where
  toInt :: Thing -> Int
  toInt = whichThing

instance Intable IntList where
  toInt :: IntList -> Int
  toInt = intLength

add1 :: (Intable a) => a -> Int
add1 x = toInt x + 1

-- Re-implementing the Eq typeclass:
class Eq' a where
  eq :: a -> a -> Bool
  neq :: a -> a -> Bool
  x `eq` y = not (x `neq` y)
  x `neq` y = not (x `eq` y)

instance Eq' Thing where
  eq :: Thing -> Thing -> Bool
  Thing1 `eq` Thing1 = True
  Thing2 `eq` Thing2 = True
  Thing3 `eq` Thing3 = True
  _ `eq` _ = False

instance Eq' Person where
  eq :: Person -> Person -> Bool
  Person x y `eq` Person u v = (x == u) && (y == v)

instance (Eq a, Eq b) => Eq' (Pair a b) where
  eq :: Pair a b -> Pair a b -> Bool
  Pair x y `eq` Pair u v = (x == u) && (y == v)

-- deriving Eq will do this type of thing for you.
-- deriving Ord will order values according to the position of the data constructor and the ordering of the fields.