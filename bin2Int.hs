--myFoldr::Foldable t => (a -> b -> b) -> b -> t a -> b
--myFoldr f v [] = v
--myFoldr f v (x:xs) = f x myFoldr f v xs
bin2IntL::[Int]->Int
bin2IntL [0] = 0
bin2IntL [1] = 1
bin2IntL v = foldl (\y x -> 2 * y + x) 0 v

bin2IntR::[Int]->Int
bin2IntR xs = foldr (\x y -> 2 * y + x) 0 (reverse xs)

bin2IntN::[Int]->Int
bin2IntN [0] = 0
bin2IntN [1] = 1
bin2IntN x = 2 * (bin2IntN (init x)) + last x