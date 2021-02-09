joinAndMap :: (a -> b -> c) -> (c -> a) -> [a] -> [b] -> [a]
joinAndMap _ _ [] [] = []
joinAndMap _ _ [] ys = []
joinAndMap _ _ as [] = []
joinAndMap f1 f2 (a:as) (y:ys) = f2 (f1 a y) : joinAndMap f1 f2 as ys