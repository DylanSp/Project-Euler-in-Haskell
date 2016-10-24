isListPalindrome :: [Char] -> Bool
isListPalindrome str = case (length str) of
    0 -> True
    1 -> True
    2 -> head str == last str
    _ -> head str == last str && (isListPalindrome $ init $ tail str)

isNumPalindrome :: Integer -> Bool
isNumPalindrome n = isListPalindrome $ show n

--findLargestPalindrome :: () -> Integer
findLargestPalindrome = maximum [ m * n | m <- [999, 998..1], n <- [m, m-1..1], isNumPalindrome (m * n)]