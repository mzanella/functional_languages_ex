--take a function and apply it to all elements of a list
myMap :: (a -> b) -> [a] -> [b]
myMap _[] = []
myMap fn (x:xn) = fn x : myMap fn xn

multipleBy2 :: Int -> Int
multipleBy2 n = n*2

sum1 :: [Int] -> [Int]
sum1 = myMap (+1)