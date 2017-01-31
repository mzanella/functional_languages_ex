import Control.Monad

--1) Define an instance of the Monad class for the type (a ->) . Remember that one has to write, instance Monad ((->)a) where...
{-instance Monad ((->) r) where
    return = pure
    g >>= f = \x -> f (g x) x
-}

-- *******************************************************************************************************************************

{-2) Given the following type of expressions  

    data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show 

that contains variables of some tipe a, show how to make this type into instances of Functor, Applicative and Monad classes. 
With the aid of an example, explain what the >>= operator for this type does. 
-}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    --fmap :: Functor f => (a->b) -> f a -> f b
    fmap f (Val x) = Val x
    fmap f (Var x) = Var (f x)
    fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var
    -- Applicative f => f (a -> b) -> f a -> f b
    -- (<*>) :: Expr (a->b) -> Expr a -> Expr b
    Var f <*> x = fmap f x
    (Add f g) <*> e = Add (f <*> e) (g <*> e)
    (<*>) _ (Val x) = Val x

instance Monad Expr where
    return = pure
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    x >>= f = case x of Val y -> Val y
                        Var x -> f x
                        Add x1 x2 -> Add (x1 >>= f) (x2 >>= f) 
-- it applies the function f to all 

-- *******************************************************************************************************************************

{-3) Rather than making a parameterized type into instances of the Funcor, Applicative and Monad classes in this order, 
in practice it is sometimes simpler to define the Functor and Applicative instances in terms of the Monad instance, 
relying on the fact that the order in which the declarations are made is not important in Haskell. 
Thus, given 
    
    instance Monad ST where 
        st >>= f = S(\s -> let (x,s') = app st s 
                           in app (f x) s') 

define the Functor and Applicative instances of ST, using the do notation.-}

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Monad ST where
        -- (>>=) :: ST a -> (a -> ST b) -> ST b
        st >>= f = S(\s -> let (x,s') = app st s 
                           in app (f x) s')
instance Functor ST where
    -- fmap:: (a->b) -> ST a -> ST b
    -- fmap g st = S (\s -> let (x,s1) = app st s 
    --                      in (g x, s1))
    fmap f st = do x <- st
                   return (f x)   

instance Applicative ST where
    pure = return

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    --stf <*> stx = S (\s -> let (f,s1) = app stf s 
    --                           (x,s2) = app stx s1 
    --                       in (f x, s2))
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x) 
