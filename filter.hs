-- enter a function or opetaion and a list  of elements 
-- return a new list according with the filter 

-- example: filter' odd [1..10] 
-- return only the odd numbers [1,3,5,7,9]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)  | p x = x : filter' p xs
                  | otherwise = filter' p xs