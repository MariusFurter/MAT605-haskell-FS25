-- Exercise 1
data CoolBool = Nope | Yup deriving (Show, Eq, Ord)

-- Exercise 2
boolToCool :: Bool -> CoolBool
boolToCool True = Yup
boolToCool False = Nope

boolToCool' :: Bool -> CoolBool
boolToCool' x
  | x = Yup
  | otherwise = Nope

coolToBool :: CoolBool -> Bool
coolToBool Yup = True
coolToBool Nope = False

-- Exercise 3
coolNot :: CoolBool -> CoolBool
coolNot Yup = Nope
coolNot Nope = Yup

coolBoth :: CoolBool -> CoolBool -> CoolBool
coolBoth Yup Yup = Yup
coolBoth _ _ = Nope

coolEither :: CoolBool -> CoolBool -> CoolBool
coolEither Nope Nope = Nope
coolEither _ _ = Yup

coolEither' :: CoolBool -> CoolBool -> CoolBool
coolEither' x y = coolNot (coolBoth (coolNot x) (coolNot y))

-- Exercise 4
coolAnd :: [CoolBool] -> CoolBool
coolAnd [] = Yup
coolAnd (x : xs)
  | x == Nope = Nope
  | otherwise = coolAnd xs

coolAnd' :: [CoolBool] -> CoolBool
coolAnd' [] = Yup
coolAnd' (Nope : xs) = Nope
coolAnd' (Yup : xs) = coolAnd xs

coolOr :: [CoolBool] -> CoolBool
coolOr [] = Nope
coolOr (Yup : xs) = Yup
coolOr (Nope : xs) = coolOr xs

-- Exercise 5
coolElem :: (Eq a) => a -> [a] -> CoolBool
coolElem e [] = Nope
coolElem e (x : xs)
  | e == x = Yup
  | otherwise = coolElem e xs

-- Exercise 6
coolAll :: (a -> CoolBool) -> [a] -> CoolBool
coolAll p [] = Yup
coolAll p (x : xs)
  | p x == Nope = Nope
  | otherwise = coolAll p xs

coolAll' :: (a -> CoolBool) -> [a] -> CoolBool
coolAll' p [] = Yup
coolAll' p (x : xs) =
  if p x == Nope
    then Nope
    else coolAll p xs

coolAny :: (a -> CoolBool) -> [a] -> CoolBool
coolAny p [] = Nope
coolAny p (x : xs)
  | p x == Yup = Yup
  | otherwise = coolAny p xs