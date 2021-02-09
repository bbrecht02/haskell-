compress :: Eq => [a] -> [a]
compress [] = [] 
compress (a:as) | a == as = rm a as 
                | otherwise = as 


rm :: Int -> [Int] -> [Int]
rm _ [] = []
rm n (x:xs) | n == x = xs
            | otherwise = x : rm n xs