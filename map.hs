--take a function and apply it to all elements of a list
myMap :: (a -> b) -> [a] -> [b]
myMap _[] = []
myMap fn (x:xn) = fn x : myMap xn