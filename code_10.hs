mult4 :: Int -> Int
mult4 x  = 4*x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == p) && (m == p)

max2 :: Int -> Int -> Int
max2 n m | n >=m = n
         | otherwise = m

max3 :: Int -> Int -> Int -> Int
max3 a b c | (a>=b) && (a>=c) = a
           | (b>=c) && (b>=a) = b
           | otherwise = c

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

power :: Int -> Int -> Int
power _ 0 = 1 
power x y = x * power x (y-1)
