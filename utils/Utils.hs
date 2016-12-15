module Utils
( olMinus
, primesToM
, pythTriples
, factorial
, choose
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


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

sumDigits :: Integer -> Integer
sumDigits n
    | n < 10    = n
    | otherwise = n `mod` 10 + sumDigits (n `div` 10)
    
intSqrt :: Integer -> Integer
intSqrt n = floor . sqrt $ (fromIntegral n :: Float)

isSquare :: Integer -> Bool
isSquare n = n == intSqrt n * intSqrt n

isPrime :: Integer -> Bool
isPrime n = n `elem` primesToM n