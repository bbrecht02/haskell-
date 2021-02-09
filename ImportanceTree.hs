-- Show -- Ord -- Eq 
data Inhabitant = Elves String | Humans String| Dwarves String| Hobbits String deriving (Show, Eq, Ord)

-- moreImportant
moreImportant:: Inhabitant -> Inhabitant -> Bool
moreImportant x y = x > y

--tree
data Tree t = Nil | Node t (Tree t) (Tree t) deriving (Show, Ord, Eq)

createImportanceTree :: [Inhabitant] -> Tree Inhabitant
createImportanceTree [] = Nil
createImportanceTree (x:xs) = createImportanceTree_aux (Node x Nil Nil) xs
         where 
                createImportanceTree_aux tree [] = tree
                createImportanceTree_aux tree (x:xs) = createImportanceTree_aux (insert tree x) xs


insert :: Tree Inhabitant -> Inhabitant -> Tree Inhabitant
insert Nil x = Node x Nil Nil
insert (Node y t1 t2 ) x
      | (x == y) = Node y t1 t2
      | (x > y) = Node y t1 (insert t2 x)
      | (x < y) = Node y (insert t1 x) t2

-- children
children :: Tree Inhabitant -> Inhabitant -> [Inhabitant]
children Nil _ = []
children (Node y t1 t2) x 
          | isin x (collapse (Node y t1 t2)) = importance x [] (collapse (Node y t1 t2))
          | otherwise = []

isin :: (Eq a) =>  a -> [a] -> Bool
isin y [] = False
isin y (x:xs)
    | y == x = True
    | otherwise = isin y xs

importance :: Inhabitant -> [Inhabitant] -> [Inhabitant] -> [Inhabitant]
importance _ acc [] = acc
importance a acc (x:xs) | moreImportant x a = importance a (acc ++ [x]) xs
                   | otherwise = importance a acc xs

collapse :: Tree t -> [t]
collapse Nil = []
collapse (Node y t1 t2) = collapse t1 ++ [y] ++ collapse t2