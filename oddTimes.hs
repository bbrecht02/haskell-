-- removes all elements that show an pair number of times
-- and return them in descending order 

oddTimes :: (Eq a, Ord a) => [a] -> [a]
oddTimes [] = []
oddTimes [a] = [a]
oddTimes (a:as) = reverse' (sort (rmPair (a:as)))

rmPair :: (Eq a) => [a] -> [a]
rmPair [] = [] 
rmPair [a] = [a]
rmPair (a:as) 
   | odd (repeatN a (a:as)) = a : rmPair as
   | pertence a (a:as) = rmPair (rm a as) 
   | otherwise = rmPair as

rm ::(Eq a) =>  a -> [a] -> [a]
rm _ [] = []
rm n (x:xs) | n == x = xs
            | otherwise = x : rm n xs

repeatN :: (Eq a) => a -> [a] -> Int
repeatN a [] = 0
repeatN x (a:as) 
    | x == a = 1 + repeatN x as
    | otherwise = repeatN x as

reverse' :: [t] -> [t]
reverse' [] = []
reverse' (a:as) = reverse' as ++ [a]

pertence :: (Eq a) => a -> [a] -> Bool
pertence x [] = False
pertence x (a:as) 
    | x == a = True
    | otherwise = pertence x as

sort :: (Ord a) => [a] -> [a]
sort []  = []
sort (a:as) = insert a (sort as)

insert :: (Ord a) => a -> [a] -> [a]
insert num [] = [num]
insert num (a:as) | num <= a = num : (a:as)
                  | otherwise = a : insert num as
