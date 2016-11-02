--set difference on ordered lists
minus :: (Ord a) => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primesToM :: Integer -> [Integer]
primesToM m = 2 : sieve [3,5..m]
  where
    sieve (p:xs) 
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
       
       
--appropriate range found manually
solution = (primesToM 120000) !! 10000