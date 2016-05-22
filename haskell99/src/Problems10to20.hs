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

decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs) = [x] ++ decodeModified xs
decodeModified (Multiple 2 x: xs) = [x] ++ decodeModified ((Single x):xs)
decodeModified (Multiple c x: xs) = [x] ++ decodeModified ((Multiple (c -1) x):xs)


-----------------PROBLEM 13 SOLVED----------------------------

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

repli :: [a] -> Int -> [a]
repli [] c = []
repli (x:[]) 1 = [x]
repli (x:[]) c = [x] ++ (repli [x] (c - 1))
repli (x:xs) c = (repli [x] c) ++ (repli (xs) c)

dropEvery :: [a] -> Int  -> [a]
dropEvery xs n = helper xs n
  where helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) i = x : helper xs (i - 1)
