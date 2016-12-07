factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

--solution = 40 choose 20
solution = factorial 40 `div` (factorial 20 * factorial 20)