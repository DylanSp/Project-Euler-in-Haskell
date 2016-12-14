number :: Integer
number = 2 ^ 1000

sumDigits :: Integer -> Integer
sumDigits n
    | n < 10    = n
    | otherwise = n `mod` 10 + sumDigits (n `div` 10)

solution :: Integer
solution = sumDigits number