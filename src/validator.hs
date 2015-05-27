toDigits :: Integer -> [Integer]
toDigits n
    | n < 0    = []
    | n < 10    = [n]
    | otherwise = (toDigits (n `div` 10)) ++ (toDigits (n `rem` 10))

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 0    = []
    | n < 10    = [n]
    | otherwise = (toDigitsRev $ n `rem` 10) ++ (toDigitsRev $ n `div` 10)

{-  Example [1,2,3,4] -> [1,4,3,8]
    base case ns is null, return empty list -}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns
    | ns == []                  = []
    | length ns `mod` 2 == 0    = [head ns * 2] ++ (doubleEveryOther $ tail ns)
    | otherwise                 = [head ns] ++ (doubleEveryOther $ tail ns)

sumDigits :: [Integer] -> Integer
sumDigits ns
    | ns == []      = 0
    | head ns > 9   = (sumDigits $ toDigits $ head ns) + (sumDigits $ tail ns)
    | otherwise     = (head ns) + (sumDigits $ tail ns)

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `rem` 10 == 0