module Problems10to20 where

data Encoded a = Single a | Multiple Int a
    deriving (Show,Eq)
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified [] = []
encodeModified (x:xs) =
                  let l = length( filter(== x) (x:xs))
                  in case l of
                   _ | (l == 1) -> [Single x]
                     | (l > 1) -> [Multiple l x]
                  ++ encodeModified (filter (/= x) xs)