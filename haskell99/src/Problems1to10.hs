module Problems1to10 where

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (_:xs) = myLast xs

