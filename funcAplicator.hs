-- apply the functions to 2 elements 

--example
--aplicador [(+), (-), (*), (/)] 8 2
-- out [10.0,6.0,16.0,4.0 ]

aplicador :: [(a -> b -> b)] -> a -> b -> [b]
aplicador [] _ _ = []
aplicador  (x:xs) a b = jres (res (funL (decom (x:xs) a)) b) ++ aplicador xs a b

--apply in the aplicador 
decom :: [(a -> b -> b)] -> a -> [(b -> b)]
decom [] _ = []
decom (a:as) y =  a y : decom as y 

funL :: [(b -> b)] -> (b -> b) 
funL (a:as) = a

res :: (b -> b) -> b -> b
res f x = f x 

jres :: b -> [b] 
jres b = [b]
