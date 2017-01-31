-- hanoi tower
-- hanoi 2 a b c == [(a,c), (a,b), (c,b)]
-- _                                                                                   _
-- __      -> __       ->           ->     _    ->  _      ->          ->     __ ->    __
-- ___        ___   _     ___ __ _     ___ __       __ ___    _ __ ___    _   ___      ___
-- a   c b    a   c b     a   c  b     a   c  b   a c  b      a c  b      a c b    a c b   
--a,b|a,c|b,c|a,b|c,a|c,b|a,b
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)  