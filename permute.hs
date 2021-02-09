--return all the permutes of a word 

permute :: (Ord a) => [a] -> [[a]]
permute [] = [[]]
permute (a:as) 
    | (lengthList (a:as)) == 2 = (a:as) : reverse' (a:as) : []
    | (lengthList (a:as)) == 1 = (a:as) : []
    | otherwise = (sort (a:as)) : (a:as) : reverse' (sort (a:as)) : [] 

split :: [a] -> [[a]]
split [] = []
split (a:as) = [a] : split as

lengthList :: [a] -> Int
lengthList [] = 0 
lengthList (x:xs) = 1 + lengthList xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (a:as) = reverse' as ++ [a]

sort :: (Ord a) => [a] -> [a]
sort []  = []
sort (a:as) = insert a (sort as)

insert :: (Ord a) => a -> [a] -> [a]
insert num [] = [num]
insert num (a:as) | num <= a = num : (a:as)
                  | otherwise = a : insert num as