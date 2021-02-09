--remove repeated itens

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove  y (x:xs) | y == x = remove y xs
                 | otherwise = x : remove y xs