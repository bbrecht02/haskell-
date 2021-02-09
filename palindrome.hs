isPalindrome :: (Eq t) => [t] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome as | as == (reverse' as) = True
                | otherwise = False