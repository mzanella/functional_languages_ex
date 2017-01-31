doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n 
   | ((length n) `mod` 2)==0 = evenDouble n
   | otherwise = oddDouble n

evenDouble :: [Integer] -> [Integer]
evenDouble n
   | (length n)==0 = []
   | otherwise = ((n !! 0)*2):(n !! 1):evenDouble (drop 2 n)

oddDouble :: [Integer] -> [Integer]
oddDouble n
   | (length n)==1 = [(n !! 0)]
   | otherwise = (n !! 0):((n !! 1)*2):oddDouble (drop 2 n)
