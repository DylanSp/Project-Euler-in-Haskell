module Utils
( olMinus
, primesToM
, pythTriples
) where

--set difference on ordered lists
olMinus :: (Ord a) => [a] -> [a] -> [a]
olMinus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : olMinus  xs  (y:ys)
           EQ ->     olMinus  xs     ys 
           GT ->     olMinus (x:xs)  ys
olMinus  xs     _     = xs


primesToM :: Integer -> [Integer]
primesToM m = 2 : sieve [3,5..m]
  where
    sieve (p:xs) 
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `olMinus` [p*p, p*p+2*p..])
       
pythTriples = [(m * m - n * n, 2 * m * n, m * m + n * n) | m <- [1..], n <- [1..m], m /= n ]