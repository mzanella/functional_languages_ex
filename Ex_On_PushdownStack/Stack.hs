import Control.Monad

type Stack=[Char]

pop :: Stack -> (Char,Stack)
pop (x:xs) = (x,xs)

push :: Char -> Stack -> ((),Stack)
push x xs = ((),x:xs)

check_par :: [Char] -> Stack -> Bool
check_par [] [] = True
check_par [] _ = False
check_par (c:cs) st
    | c == '(' = let (w,st') = (push ')' st) in (check_par cs st')
    | c == ')' = let (w,st') = (pop st) in (check_par cs st')
    | otherwise = check_par cs st

newtype ST a = S (Stack->(a, Stack))

app :: ST a -> Stack -> (a, Stack)
app (S st) x = st x 

instance Functor ST where 
    -- fmap :: (a - >b) - > ST a - > ST b 
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where 
    -- pure :: a - > ST a 
    pure x = S ( \s -> (x, s)) 
    -- (<*>) :: ST (a - >b) - > ST a - > ST b 
    stf <*> stx = S (\s -> let (f, s') = app stf s
                               (x, s'') = app stx s' 
                               in (f x, s'')) 

instance Monad ST where 
    return = pure
    -- (>>=):: ST a - > ( a - > ST b) - > ST b 
    st >>= f = S(\s -> let (x,s')= app st s in app (f x) s') 

pop1 :: ST Char
pop1 = S (\xs-> (head xs, tail xs))
                
push1 :: Char -> ST ()
push1 c = S (\stack-> ((), (c:stack)))

emptyStack :: ST Bool
emptyStack = S(\stack->((length stack == 0), stack))

check_par1 :: [Char] -> ST Bool
check_par1 [] = emptyStack
check_par1 ('(':cs) = do x <- push1 '('
                         check_par1 cs
check_par1 (')':cs) = do x <- pop1
                         check_par1 cs
check_par1 (_:cs) = check_par1 cs

check_par2 :: [Char] -> ST Bool
check_par2 [] = emptyStack
check_par2 ('(':cs) = (push1 '(') >>= \x -> check_par1 cs
check_par2 (')':cs) = pop1 >>= \x -> check_par1 cs
check_par2 (_:cs) = check_par1 cs