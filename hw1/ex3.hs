toDigits :: Integer -> [Integer]
toDigits n
   | n<=0 = []
   | otherwise = (toDigits (n `div` 10))++[(n `mod` 10)]

sumDigits :: [Integer] -> Integer
sumDigits n
   | (length n)==0 = 0
   | otherwise = if (head n)>9 
                 then sumDigits(toDigits (head n)) + sumDigits (tail n)
                 else (head n) + sumDigits (tail n)
