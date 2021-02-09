-- remove the nth large number

removeLargeN :: Int -> [Int] -> [Int]
removeLargeN 0 xs = xs
removeLargeN _ [] = []
removeLargeN n xs = rm (findNlarge n xs) xs

findNlarge :: Int -> [Int] -> Int
findNlarge n xs 
        | n == 1 = max' xs 
        | otherwise = findNlarge (n-1) (rmMax xs)

rmMax :: [Int] -> [Int]
rmMax [] = []
rmMax xs = rm (max' xs) xs  

rm :: Int -> [Int] -> [Int]
rm _ [] = []
rm n (x:xs) | n == x = xs
            | otherwise = x : rm n xs

max' :: [Int] -> Int
max' [] = 0
max' [x] =  x
max' (x:xs) | max' xs >= x = max' xs
            | otherwise = x 