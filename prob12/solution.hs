nthTriangleNumber :: Integer -> Integer
nthTriangleNumber n = n * (n - 1) `div` 2

triangleNumbers :: [Integer]
triangleNumbers = map nthTriangleNumber [1..]

intSqrt :: Integer -> Integer
intSqrt n = floor . sqrt $ (fromIntegral n :: Float)

isSquare :: Integer -> Bool
isSquare n = n == intSqrt n * intSqrt n

numFactors :: Integer -> Integer
numFactors n = fromIntegral $ (2 * length [f | f <- [1..(intSqrt n)], n `mod` f == 0]) - (if isSquare n then 1 else 0)

triangularFactorNums :: [(Integer, Integer)]
triangularFactorNums = zip (map numFactors triangleNumbers) triangleNumbers

solution = snd $ head $ dropWhile (\ (f, s) -> f <= 500) triangularFactorNums