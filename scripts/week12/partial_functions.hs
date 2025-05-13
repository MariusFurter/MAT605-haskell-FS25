-- data Maybe a = Nothing | Just a

-- Partial functions that might not return a value

head' :: [a] -> a
head' [] = error "Empty list"
head' (x : xs) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

-- Functions that might return several values

sqrt' :: Double -> [Double]
sqrt' 0.0 = [0.0]
sqrt' x = [-sqrt x, sqrt x]

add3Head :: [Int] -> Int
add3Head xs = head xs + 3

-- Doesn't work
-- safeAdd3Head :: [Int] -> Int
-- safeAdd3Head xs = safeHead xs + 3

-- Higher level functions
fToMaybe :: (a -> b) -> Maybe a -> Maybe b
fToMaybe f Nothing = Nothing
fToMaybe f (Just x) = Just (f x)

-- This is in fact already implemented as fmap

fToList :: (a -> b) -> [a] -> [b]
fToList f [] = []
fToList f (x : xs) = f x : fToList f xs

-- This is in fact already implemented as map and fmap

safeAdd3Head :: [Int] -> Maybe Int
safeAdd3Head xs = fToMaybe (+ 3) (safeHead xs)

-- Using fmap
safeAdd3Head' :: [Int] -> Maybe Int
safeAdd3Head' xs = fmap (+ 3) (safeHead xs)

sqrtAdd3 :: Double -> [Double]
sqrtAdd3 x = fToList (+ 3) (sqrt' x)

-- equivalently fToList = map = fmap

-- Converting values to Maybe / Lists
toMaybe :: a -> Maybe a
toMaybe x = Just x

toList :: a -> [a]
toList x = [x]

-- flattening
flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just x)) = Just x

flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList (x : xs) = x ++ flattenList xs