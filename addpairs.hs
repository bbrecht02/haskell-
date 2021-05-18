-- enter a list of tuples
-- return a list with the sum of the elements of the each tuple

addpair :: (Int,Int) -> Int
addpair (x,y) = x+y

addPairs :: [(Int,Int)] -> [Int]
addPairs addpair = [x+y | (x,y) <- addpair, x < y]