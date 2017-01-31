myFilter :: (a->Bool)-> [a] -> [a]
myFilter pred xs = [z | z<-xs, pred z]

myTakeWhile :: (a->Bool) -> [a] -> [a]
myTakeWhile pred [] = []
myTakeWhile pred (x:xs) | pred x = x:(myTakeWhile pred xs)
                        | otherwise = []

myDropWhile :: (a->Bool) -> [a] -> [a]
myDropWhile pred [] = []
myDropWhile pred (x:xs) | pred x = myDropWhile pred xs
                        | otherwise = x:xs

myFoldr :: (a->b->b) -> b -> [a] ->b
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)

sumWithFoldr :: (Num a) => [a] -> a 
sumWithFoldr xs = myFoldr (+) 0 xs

orWithFoldr :: [Bool] -> Bool
orWithFoldr xs = myFoldr (||) False xs

lengthWithFoldr :: [a] -> Int
lengthWithFoldr xs = myFoldr ( \_ x -> 1 + x) 0 xs

reverseWithFoldr :: [a] -> [a]
reverseWithFoldr xs = myFoldr (\y ys -> ys ++ [y]) [] xs

myFoldl :: (a->b->b) -> b -> [a] ->b
myFoldl f v [] = v
myFoldl f v (x:xs) = myFoldl f (f x v) xs

sumWithFoldl :: Num a => [a] -> a
sumWithFoldl xs = myFoldl (+) 0 xs

lengthWithFoldl :: [a] -> Int
lengthWithFoldl xs = myFoldl (\_ x -> x + 1) 0 xs

main = do print (lengthWithFoldl [1,3,4] == lengthWithFoldr [1,3,4])
