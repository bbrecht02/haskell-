sort :: (Ord a) => [a] -> [a]
sort []  = []
sort (a:as) = insert a (sort as)

insert :: (Ord a) => a -> [a] -> [a]
insert num [] = [num]
insert num (a:as) | num <= a = num : (a:as)
                  | otherwise = a : insert num as
