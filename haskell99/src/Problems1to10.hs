module Problems1to10 where

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (_:xs) = myLast xs


myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:_:[]) = Just x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) c = elementAt xs (c - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


data NestedList a = Elem a | List[NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List (xs))

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress (filter (/= x) xs))

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (filter (==x) xs) : pack (filter (/=x) xs)

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = (length (filter (== x) (x:xs)),x) : encode (filter (/= x) xs)