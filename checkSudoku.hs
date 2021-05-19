checkN :: [Int] -> Bool 
checkN [] = True
checkN (a:as) 
    | a > 9 = False
    | a < 1 = False
    | otherwise = checkN as

-- aux
pertence :: (Eq a) => a -> [a] -> Bool
pertence x [] = False
pertence x (a:as) 
    | x == a = True
    | otherwise = pertence x as

headList' :: [[a]] -> [a]
headList' [] = []
headList' (a:as) = a


repeat' :: [Int] -> Bool 
repeat' [] = True
repeat' (a:as) 
    | pertence a as = False
    | otherwise = repeat' as

-- check lines
checkLi' :: [[Int]] -> Bool
checkLi' [] = True
checkLi' [[]] = True
checkLi' (a:as) 
    | repeat' (headList' (a:as))  = checkLi' as 
    | otherwise = False

 
-- COLUMNS
-- make column 
columns :: [[Int]] -> [[Int]]
columns [] = []
columns [[]] = [[]]
columns (a:b:c:d:e:f:g:h:i:[]) = makeC a b c d e f g h i  

makeC :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
makeC [] [] [] [] [] [] [] [] [] = []
makeC _ _ _ _ _ _ _ _ [] = []
makeC _ _ _ _ _ _ _ [] _ = []
makeC _ _ _ _ _ _ [] _ _ = []
makeC _ _ _ _ _ [] _ _ _ = []
makeC _ _ _ _ [] _ _ _ _ = []
makeC _ _ _ [] _ _ _ _ _ = []
makeC _ _ [] _ _ _ _ _ _ = []
makeC _ [] _ _ _ _ _ _ _ = []
makeC [] _ _ _ _ _ _ _ _ = []
makeC (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) = [a:b:c:d:e:f:g:h:i:[]] ++ makeC as bs cs ds es fs gs hs is  

-- check columns
checkCo' :: [[Int]] -> Bool
checkCo' [] = True
checkCo' [[]] = True
checkCo' as 
    | checkLi' (columns as) && checkLi' as = True 
    | otherwise = False


-- GRID 3X3
--remove 3 itens de uma lista
remove :: (Eq a) => [a] -> [a]
remove [] = []
remove (a:b:c:as) = as


f1 :: [Int] -> [Int] -> [Int] -> [[Int]]
f1 [] [] [] = []
f1 (a:b:c:as) (d:e:f:ds) (g:h:i:gs) = [a:b:c:d:e:f:g:h:i:[]] ++ f1 as ds gs 


ff :: [[Int]] -> [[Int]]
ff [] = []
ff (x:y:z:zs) = f1 x y z ++ ff zs   
-----

concL :: [[a]] -> [a]
concL [] = []
concL (y:ys) = y ++ concL ys

--check list and columns
checkListandColumns :: [[Int]] -> Bool 
checkListandColumns [] = False
checkListandColumns [[]] = False
checkListandColumns xs 
    | checkLi' xs && checkCo' xs = True
    | otherwise = False

check3x3 :: [[Int]] -> Bool 
check3x3 [] = False
check3x3 [[]] = False
check3x3 xs 
    | pertence [] xs = False
    | checkListandColumns xs && checkLi' (ff xs) = True
    | otherwise = False


--  (MAIN)
checkSudoku :: [[Int]] -> Bool
checkSudoku [] = False
checkSudoku xs 
    | checkN (concL xs) && check3x3 xs = True 
    | otherwise = False
