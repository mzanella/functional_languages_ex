toDigits :: Integer -> [Integer]
toDigits n
   | n<=0 = []
   | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
   | n<=0 = []
   | otherwise = (n `mod` 10):toDigitsRev (n `div` 10)
-- toDigitsRev n = reverse (toDigits n)
