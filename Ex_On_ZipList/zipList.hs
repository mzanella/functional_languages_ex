{-
Z[(+), (*), (\)] <*> Z[1, 2, 3] <*> Z[2, 3] ----> [1+2, 2*3]    
-}
import Control.Monad

newtype ZipList a = Z [a] deriving (Show,Eq)

instance Functor ZipList where
    -- Functor f => (a->b) -> f a -> f b
    -- fmap :: (a->b) -> ZipList a -> ZipList b
    -- functor laws:
    -- 1- fmap (id) (Z [2]) = Z [2]
    -- 2- (fmap ((+2).(+3)) (Z [2])) == (fmap ((+2).(+3)) (Z [2]))
    fmap _ (Z []) = Z []
    fmap g (Z (x:xs)) = Z [g x]

instance Applicative ZipList where
    -- a -> fa 
    --pure :: a -> ZipList a
    pure x = Z (repeat x)

    -- Applicative f => f (a -> b) -> f a -> f b
    -- (<*>) :: ZipList (a->b) -> ZipList a -> ZipList b
    (<*>) (Z []) _ = Z []
    (<*>) _ (Z []) = Z []
    (<*>) (Z fs) (Z xs) = Z [f x | (f,x)<-(zip fs xs) ]
    
    -- applicatives laws
    -- 1- pure id <*> Z[2] = Z[2]
    -- 2- pure ((Z) ([2,3])) = pure (Z) <*> pure ([2,3])
    -- 3-
    -- 4-


-- pure(++)<*>(pure(++)<*>(pure(:[])<*>[1,2,3])<*>(pure(:[])<*>[1,2,3]))<*>(pure(:[])<*>[1,2,3])
-- pure(++)<*>(pure(++)<*>(pure(:[])<*>Z[1,2,3])<*>(pure(:[])<*>Z[1,2,3]))<*>(pure(:[])<*>Z[1,2,3])
-- (pure map)<*>(pure(+)<*>Z[1..])<*>Z[[0,0],[0,0]]

ex2a :: [a]->[a]->[a]->[[a]]
ex2a [] _ _ = []
ex2a _ [] _ = []
ex2a _ _ [] = []
-- ex2a xs ys zs = pure(++)<*>(pure(++)<*>(pure(:[])<*>xs)<*>(pure(:[])<*>ys))<*>(pure(:[])<*>zs)
ex2a xs ys zs = (pure (:) <*> xs) <*> ((pure (:) <*>ys) <*> (pure (:[]) <*> zs))

ex2b :: [a]->[a]->[a]->[[a]]
ex2b [] _ _ = []
ex2b _ [] _ = []
ex2b _ _ [] = []
-- ex2b xs ys zs = ex2Aux (pure(++)<*>(pure(++)<*>(pure(:[])<*>(Z xs))<*>(pure(:[])<*>(Z ys)))<*>(pure(:[])<*>(Z zs)))
ex2b xs ys zs = ex2Aux ((pure (:) <*> (Z xs)) <*> ((pure (:) <*> (Z ys)) <*> (pure (:[]) <*> (Z zs))))

ex2Aux :: ZipList a->[a]
ex2Aux (Z xss) = xss

ex2c :: [[Int]]->[[Int]]
ex2c xss = ex2Aux ((pure map)<*>(pure(+)<*>Z[1..])<*>(Z xss))
