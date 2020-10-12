--Ex1
data LExpr = Var Char | Lambda Char LExpr | App LExpr LExpr 
instance Show LExpr where
    show (Var c) = [c] 
    show (Lambda c e) = "\0955" ++ [c] ++ "."++ (show e)
    show (App e1 e2) = (show e1) ++ " "++(show e2)

--Ex2
vars :: LExpr -> [Char]
vars (Var c) = [c]
vars (Lambda c e) = [c] ++ (vars e)
vars (App e1 e2) = (vars e1) ++ (vars e2)

--Ex3
reducible :: LExpr -> Bool
reducible (Var c) = False
reducible (Lambda c e) = (reducible e)
reducible (App (Lambda _ _) e2) = True
reducible (App _ e2) = (reducible e2)

--Ex4
rename :: Char -> Char -> LExpr -> LExpr
rename c1 c2 (Var c)
 |c == c2 = (Var c1)
 |otherwise = (Var c)
rename c1 c2 (Lambda c e)
 |c == c2 = (Lambda c1 (rename c1 c2 e))
 |otherwise = (Lambda c (rename c1 c2 e))
rename c1 c2 (App e1 e2) = (App (rename c1 c2 e1) (rename c1 c2 e2))

