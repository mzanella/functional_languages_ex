import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P parser) input = parser input

instance Functor Parser where
    fmap f parser = P(\input -> case parse parser input of 
                                    [] -> []
                                    [(v,other)] -> [(f v,other)])

instance Applicative Parser where
    pure v = P(\input -> [(v,input)])
    parserf <*> parserx = P(\input -> case parse parserf input of 
                                        [] -> []
                                        [(f,other)] -> parse (fmap f parserx) other)

instance Monad Parser where
    return = pure
    parser >>= f = P(\input -> case parse parser input of 
                                [] -> []
                                [(v,other)] -> parse (f v) other)

instance Alternative Parser where
    empty = P(\input->[])
    parserx <|> parsery = P(\input -> case parse parserx input of 
                                        [] -> parse parsery input
                                        [(v,other)] -> [(v, other)])
    some x = pure (:) <*> x <*> many x
    many x = some x <|> pure []

item :: Parser Char
item = P(\input -> case input of 
                        [] -> []
                        (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if (predicate x) 
                       then return x
                       else empty   

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-1 * n)
         <|>
         nat

token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

identifier :: Parser String
identifier = token ident

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- 1) Define a parser 
--          comment :: Parser () 
--    for ordinary Haskell comments that begin with -- and extend to the end of the current line, which is represented by the 
--    control character '\n'.
comments :: Parser ()
comments = do space
              string "--"
              many (do char ' '
                       <|>
                       sat isAlphaNum)
              char '\n'
              return ()

-- 2) Define a suitable tipe Expr for arithmetic expressions and modify the parser for espressions to have
--    expr:: Parser Expr. Hint: Expr should represent a derivation tree of the input expression.

data Expr = ADD Expr Expr | SUB Expr Expr | NUM Int deriving(Show, Eq)

parserNum :: Parser Expr
parserNum = do num <- integer
               return (NUM num)

parserOPA :: Parser (Expr->Expr->Expr)
parserOPA = do symbol "+"
               return ADD
            <|>
            do symbol "-"
               return SUB

parserTerm :: Parser Expr
parserTerm = do symbol "("
                expr <- parserExpr
                symbol ")"
                return expr
                <|>
                do num <- parserNum
                   return num

parserExpr :: Parser Expr
parserExpr = do term <- parserTerm
                do op <- parserOPA 
                   expr <- parserExpr 
                   return (op term expr)
                   <|>
                   return term

{-

parserFactor :: Parser Expr
parserFactor = do symbol "("
                  expr <- parserExpr
                  symbol ")"
                  return expr
                  <|>
                  do num <- parserNum
                     return num

parserOPM :: Parser (Expr->Expr->Expr)
parserOPM = do symbol "*"
               return MULT
            <|>
            do symbol "/"
               return DIV

parserTerm :: Parser Expr
parserTerm = do factor <- parserFactor
                do op <- parserOPm
                   term <- parserExpr 
                   return (op factor term)
                   <|>
                   return factor

parserExpr :: Parser Expr
parserExpr = do term <- parserTerm
                do op <- parserOPA 
                   expr <- parserExpr 
                   return (op term expr)
                   <|>
                   return term
-}


             
-- 3) Consider expressions build up from natural numbers using a subtraction that is assumed to associate to the left.
--    a. Translate this description directly into a grammar.
--    b. Implement this grammar as a parser expr:: Parser Int.
--    c. What is the problem with this parser?
--    d. Show how it can be fixed. Hint: rewrite the parser using the repetition primitive many and the library function foldl.             
   
-- a. expr ::= expr - term | term
--    term ::= (expr) | int
--    int ::= 0|1|2...

-- b.
-- parserSubTerm :: Parser Int
-- parserSubTerm = do symbol "("
--                    expr <- parserSubExpr
--                    symbol ")"
--                    return expr
--                    <|>
--                    do num <- int
--                       return num

-- parserSubExpr :: Parser Int
-- parserSubExpr = do expr <- parserSubExpr
--                    symbol "-" 
--                    term <- parserSubTerm 
--                    return (expr - term)
--                 <|>
--                 do term <- parserSubTerm
--                    return term

--c. It doesn't work because it loops indefinetly

--d. foldl :: (a -> b -> a) -> a -> [b] -> a
--   foldl f z []     = z                  
--   foldl f z (x:xs) = foldl f (f z x) xs

parserSubTerm :: Parser Int
parserSubTerm = do symbol "("
                   expr <- parserSubExpr
                   symbol ")"
                   return expr
                   <|>
                   do num <- int
                      return num

parserSubExpr :: Parser Int
parserSubExpr = do n <- parserSubTerm 
                   ns <- many (do symbol "-" 
                                  parserSubTerm)
                   return (foldl (-) n ns)

-- 4) Define an expression fibs :: [Integer] that generates the infinite sequence of Fibonacci numbers:
--          0, 1, 1, 2, 3, 5, 8, 13,...
--    using the following simple procedure:
--      *the first two numbers are 0 and 1;
--      *the next number is the sum of the previous two;
--      *return to second step.
--    Hint: it may be useful to use the functions zip and tail.
fibs :: [Integer]
fibs = genFibs [0..]

genFibs (0:1:_) = 0 : 1 : (genFibs (1:1:[]))
genFibs (p1:p2:_) = let n = p1 + p2
                    in p2 : (genFibs (p2:n:[]))

{-
    fibs :: [Integer]
    fibs = map fib [0..]
    
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
-}