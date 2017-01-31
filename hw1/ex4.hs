toDigits :: Integer -> [Integer]
toDigits n
   | n<=0 = []
   | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
   | n<=0 = []
   | otherwise = (n `mod` 10):toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n 
   | ((length n) `mod` 2)==0 = doubbler n
   | otherwise = head n:doubbler (tail n)

doubbler :: [Integer] -> [Integer]
doubbler n
   | (length n)==0 = []
   | otherwise = ((n !! 0)*2):(n !! 1):doubbler (drop 2 n)

sumDigits :: [Integer] -> Integer
sumDigits n
   | (length n)==0 = 0
   | otherwise = if (head n)>9 
                 then sumDigits(toDigits (head n)) + sumDigits (tail n)
                 else (head n) + sumDigits (tail n)

validate :: Integer -> Bool
validate n = ((sumDigits(doubleEveryOther (toDigits n))) `mod` 10)==0
