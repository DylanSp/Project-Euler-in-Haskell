import Data.List (find)
import Data.Maybe (fromMaybe)

--euler's formula for Pythagorean triples: a = m^2 - n^2, b = 2mn, c = m^2 + n^2

pythTriples = [(m * m - n * n, 2 * m * n, m * m + n * n) | m <- [1..], n <- [1..m], m /= n ]

tuple3product :: (Num a) => (a, a, a) -> a
tuple3product (a, b, c) = a * b * c

solution = tuple3product $ fromMaybe (0, 0, 0) (find (\(a, b, c) -> a + b + c == 1000) pythTriples)