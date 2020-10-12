--Ec1. A dictionary is a collection of key-value pairs, which we can represent in 
--Haskell as a list of pairs (String,Integer) where String is the key type and Integer is the value type:
type Dict = [(String,Integer)]
--Implement the function valueOf which takes a key and a dictionary, and returns the associated value. 
--It is guaranteed that the value exists.
d = [("x",3), ("are",3),("ioana",5),("haskell",7)]

valueOf :: String -> Dict -> Integer
valueOf str [] = -1
valueOf str ((k,i) : ds)
              |k == str = i
              |otherwise = valueOf str ds

--Ex. Implement the function ins which takes a key s, a value i, 
--a dictionary d and updates the value of s in the dictionary, if the value exists, 
--or adds the key-value pair, otherwise. 
--For instance ins “x” 1 [(“x”,0)] = [(“x”,1)] and ins “x” 1 [(“y”,0)] = [(“x”,1),(“y”,0)].

ins :: String -> Integer -> Dict -> Dict
ins s i [] = [(s,i)]
ins s i ((k,v) : ds)
    |s == k = ((k,i) : ds)
    |otherwise = (k,v):(ins s i ds)
  
--Ex3. Consider the type PExpr of program expressions defined in the lecture. 
---Implement a function for showing PExpr values:

data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 
 
show_pexpr :: PExpr -> String  
show_pexpr (Val v) = show v
show_pexpr (Var v) = v
show_pexpr (e :+: e') = (show_pexpr e)++ "+" ++(show_pexpr e')

           
--Ex4. Implement a function which takes a dictionary of program variables, a program expression PExpr, and evaluates it. 
--For instance: eval_pexpr [(“x”,2),(“y”,1)] ( (Var “x”) :+: (Var “y”) ) returns 3.

eval_pexpr :: Dict -> PExpr -> Integer
eval_pexpr d (Val v) = v
eval_pexpr d (Var v) = (valueOf v d)
eval_pexpr d (e :+: e') = (eval_pexpr d e) + (eval_pexpr d e')


--Ex5. Consider the type BExpr of boolean expressions defined in the lecture. Implement a function for displaying values of BExpr:

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 
instance Show PExpr where
    show = show_pexpr
 
instance Show BExpr where
    show = show_bexpr  
 
show_bexpr :: BExpr -> String
show_bexpr (e :==: e') = (show e)++ "==" ++(show e')
show_bexpr (e :<: e') = (show e)++ "<" ++(show e')
show_bexpr (Not e) = "!" ++ (show e)
show_bexpr (e :&&: e') = (show e)++ "&&" ++(show e')



    
--Ex6. Write a function which, given a dictionary, evaluates boolean conditions BExpr:

eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr d (e :==: e') = ((eval_pexpr d e) == (eval_pexpr d e'))
eval_bexpr d (e :<: e') = ((eval_pexpr d e) < (eval_pexpr d e'))
eval_bexpr d (Not e) = not(eval_bexpr d e)
eval_bexpr d (e :&&: e') = (eval_bexpr d e) && (eval_bexpr d e')

type Var = String
data Prog = PlusPlus Var |        -- x++;
            Var :=: PExpr |     -- x = <expr>;
            DeclareInt Var |      -- int x;
            Begin Prog Prog |     -- <p> <p'>
            While BExpr Prog |     -- while (<expr>) { <p> }
            If BExpr Prog Prog      -- if (<expr>) { <p> } else { <p'> }   
 
show_p :: Prog -> String
show_p (PlusPlus v) = v++"++;\n"
show_p (x :=: e) = x++"="++(show_pexpr e)++";\n"
show_p (DeclareInt x) = "int "++x++";\n"
show_p (Begin p p') = (show_p p)++(show_p p')
show_p (While e p) = "while ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n"
show_p (If e p p') = "if ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n else {\n"++(show_p p')++"}\n"
 
instance Show Prog where
  show = show_p

--Ex7. Define the program:
{-}
int x;
x++;
while (x < 100){
   x = x + 1
}
-}

p = Begin (DeclareInt "x") $
    Begin (PlusPlus "x")$
    While ((Var "x") :<: (Val 10)) 
    (("x":=: ((Var "x") :+: (Val 1))))


--Ex8. Define a function eval which takes a dictionary and a program, and evaluates the program under 
--the given dictionary. The function will return an updated dictionary. For instance:

--eval [("x",0)] (PlusPlus "x") = [("x",1)]
--eval [("x",1)] ("x" :=: ((Var "x") :+: (Val 1))) = [("x",2)]
--eval [] (DeclareInt "x") = [("x",0)]
 
eval :: Dict -> Prog -> Dict
eval d (PlusPlus x) = (ins x ((valueOf x d) + 1) d)
eval d (DeclareInt x) = (ins x 0 d)
eval d (x :=: e) = (ins x (eval_pexpr d e) d)
eval d (Begin p p') = eval (eval d p) p'
eval d (While e p) = bucla p e d
eval d (If e p p') = ( if ( (eval_bexpr d e) == True ) then (eval d p) else (eval d p'))

bucla p cond d 
 |eval_bexpr d cond == True = (bucla p cond (eval d p))
 |otherwise = d
--Add the following error-handling type to your program:

data Result a = Error String |  -- a value of type (Result a) is an error, or
                Value a         -- an actual value, wrapped by the data constructor Value
                deriving Show
                
--Ex9. Implement the function check which takes a list of defined variables, a program p, and returns an 
--updated list of variable definitions, if p does not contain undeclared variables, and an error message, otherwise.

{-For instance:
check [] (DeclareInt "x") = Value ["x"]
check [] (PlusPlus "x") = Error "x is undefined"
check ["x"] (PlusPlus "x") = ["x"]-}
verify [] var = False
verify l var
  |head l == var = True
  |otherwise = verify (tail l) var

check :: [Var] -> Prog -> Result [Var]
check l (DeclareInt x) = if (verify l x ) == False then Value [x] else Value l
check l (PlusPlus x) = if (verify l x ) == False then Error "Error" else Value l

