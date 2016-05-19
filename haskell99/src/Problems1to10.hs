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