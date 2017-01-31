--if the number is even return chain for n/2 else chain for 3*n+1
myEven :: Int -> Bool
myEven n
    | (n `mod` 2 == 0) = True
    | otherwise = False

myOdd :: Int -> Bool
myOdd n = not(myEven n)

collazChain :: Int -> [Int]
collazChain n
    | n < 0 = error "not possible"
    | n == 1 = [1]
    | myEven n == True = n:collazChain(n `div` 2)
    | otherwise = n:collazChain (3*n+1)