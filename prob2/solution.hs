fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

    
evenValuedFiboSum :: Int -> Int
evenValuedFiboSum n = sum $ filter even $ takeWhile (<=n) fibs