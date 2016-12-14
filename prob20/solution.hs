factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

sumDigits :: Integer -> Integer
sumDigits n
    | n < 10    = n
    | otherwise = n `mod` 10 + sumDigits (n `div` 10)
    
solution = sumDigits $ factorial 100