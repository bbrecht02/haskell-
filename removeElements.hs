-- return all the inputs elements without repetition 

rm :: (Eq a) => [a] -> [a]
rm [] = []
rm (a:as) | checkQ a as == 0 = a : rm as
          | otherwise = rm (rmT (a:as))  

rmT :: (Eq a) => [a] -> [a]
rmT [] = []
rmT (x:xs) = xs   

 
checkQ :: (Eq a) => a -> [a] -> Int 
checkQ a [] = 0
checkQ a (x:xs) | a == x = 1+ checkQ a xs
                | otherwise = checkQ a xs





-------
removeDups :: (Eq a) => [[a]] -> [a]
removeDups [[]] = [] 
removeDups ys = rm (concL ys) 

-- apply in the remove 
concL :: (Eq a) => [[a]] -> [a]
concL [] = []
concL (y:ys) = y ++ concL ys   
