-- enter a list of tuples
-- Return a list with the max elements of each tuple 


maxElem :: [(Int,Int)] -> [Int]
maxElem [] = []
maxElem xs = mymap (max') xs


max' :: (Int, Int) -> Int
max' (a,b) | a > b = a
           | otherwise = b

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] =[]
mymap f (x:xs) = f x : mymap f xs