import           Data.List (sortBy)
import           Data.Ord (comparing)

collatz :: Integer -> [Integer]
collatz n 
    | n == 1            = [1]
    | n `mod` 2 == 0    = n : collatz (n `div` 2)
    | otherwise         = n : collatz (3 * n + 1)
    
collatzSequences :: Integer -> [[Integer]]
collatzSequences n = map collatz [1..n]

collatzSequenceLengths :: Integer -> [(Integer, Int)] 
collatzSequenceLengths n = zip [1..n] (map length (collatzSequences n))

solution = last $ sortBy (comparing snd ) $ collatzSequenceLengths 999999
--solution = maximumBy . comparing . snd $ collatzSequenceLengths 999999