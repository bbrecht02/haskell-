flat :: [[Int]] -> [Int]
flat [] = []
flat (a:as) = a ++ flat as

count :: Int -> [Int] -> Int
count _ [] = 0
count x (y:ys) | x == y = 1 + count x ys
               | otherwise = count x ys


remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove x (y:ys) | x == y = remove x ys
                | otherwise = y : remove x ys


countList :: [Int] -> [(Int,Int)]
countList [] = []
countList (a:as) = 
	    (a, count a (a:as)) : countList (remove a as)

group :: [[Int]] -> [(Int,Int)]
group xs = countList (flat xs)