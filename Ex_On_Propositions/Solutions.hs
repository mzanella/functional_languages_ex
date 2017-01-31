module Solutions(vars, ttables,solutions, Prop(..)) where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- data una formula proposizionale F, rappresentata da un valore del tipo Prop deve
-- restituire la lista delle variabili proposizionali distinte che sono contenute in F
vars :: Prop -> [Char]
vars (Const boolean) = []
vars (Var x) = [x]
vars (Not prop) = vars prop
vars (And prop1 prop2) = variablesFromFirstProp ++ [z | z<-variablesFromSecondProp, not (checkIfPresent variablesFromFirstProp z)]
                         where variablesFromFirstProp = vars prop1
                               variablesFromSecondProp = vars prop2
vars (Imply prop1 prop2) = variablesFromFirstProp ++ [z | z<-variablesFromSecondProp, not (checkIfPresent variablesFromFirstProp z)]
                           where variablesFromFirstProp = vars prop1
                                 variablesFromSecondProp = vars prop2

checkIfPresent :: [Char] -> Char -> Bool
checkIfPresent [] _ = False
checkIfPresent xs char = (length [z | z<-xs, z==char])>0 --x==char || checkIfPresent xs char


-- data una lista L di variabili proposizionali produce la lista di tutte le
-- sostituzioni di valori di verità True/False per le variabili di L
ttables :: [Char] -> [[(Char,Bool)]]
ttables [] = []
ttables [x] = [[(x,True)],[(x,False)]]
ttables (x:xs) = myZip [(x,True),(x,False)] (ttables xs)

myZip :: [a] -> [[a]] -> [[a]]
myZip [] yss = []
myZip (x:xs) yss = conc ++ (myZip xs yss)
                   where conc = [x:ys | ys <- yss]

-- considera ogni assegnazione di valori di verità per le variabili della formula 
-- (primo parametro) e valuta se la formula è vera o no per quell'assegnazione. Deve compiere 
-- questa operazione per ogni assegnazione e restituisce True sse la formula è sempre vera
solutions :: Prop -> [[(Char,Bool)]]-> Bool
solutions _ [] = True
solutions prop (xs:xss) = (evaluateProp prop xs) && (solutions prop xss)

evaluateProp :: Prop -> [(Char, Bool)] -> Bool
evaluateProp (Const bool) _ = bool
evaluateProp (Var x) truthValues = snd (head [z | z<-truthValues, fst z == x])
evaluateProp (Not prop) truthValues = not (evaluateProp prop truthValues)
evaluateProp (And prop1 prop2) truthValues = (evaluateProp prop1 truthValues) && (evaluateProp prop2 truthValues)
evaluateProp (Imply prop1 prop2) truthValues = if not (evaluateProp prop1 truthValues) then True else (evaluateProp prop2 truthValues)