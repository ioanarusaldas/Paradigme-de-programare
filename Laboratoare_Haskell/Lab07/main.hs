{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


data Extended = Infinity | Value Integer 

-- 1
instance Eq Extended where

    Infinity == Infinity =  True
    Infinity == (Value b) = False
    (Value a) == Infinity = False
    (Value a) == (Value b) = (a == b)

-- 2]
{-
-- am comentat doarece Not apare si la exercitiul 4
data Formula a = Atom a |
                 Or (Formula a) (Formula a) |
                 And (Formula a) (Formula a) |
                 Not (Formula a)


instance Eq a => Eq (Formula a) where
    (Atom a) == (Atom b) = (a == b)
    (Or a b) == (Or c d) = (a == c) && (b == d)
    (And a b) == (And c d) = (a == c) && (b == d)
    (Not a) == (Not b) = a == b
    _ == _ = False
-}
-- 3

data ExtendedPlus = PInfinity | MInfinity | ValueP Integer

instance Num ExtendedPlus where
    -- Plus
    PInfinity + MInfinity = ValueP 0
    PInfinity + _ = PInfinity

    MInfinity + PInfinity = ValueP 0
    MInfinity + _ = MInfinity

    ValueP x + ValueP y = ValueP (x + y)
    ValueP x + PInfinity = PInfinity
    ValueP x + MInfinity = MInfinity
    -- Minus

    PInfinity - PInfinity = ValueP 0
    PInfinity - _ = PInfinity

    MInfinity - MInfinity = ValueP 0
    MInfinity - _ = MInfinity

    ValueP x - ValueP y = ValueP (x - y)
    ValueP x - PInfinity = MInfinity
    ValueP x - MInfinity = PInfinity

    -- Inmultire
    PInfinity * ValueP x = if x < 0 then MInfinity else PInfinity
    PInfinity * PInfinity = PInfinity
    PInfinity * MInfinity = MInfinity

    MInfinity * ValueP x = if x < 0 then PInfinity else MInfinity
    MInfinity * _ = MInfinity

    ValueP x * ValueP y = ValueP (x * y)
    ValueP x * PInfinity = if x < 0 then MInfinity else PInfinity
    ValueP x * MInfinity = if x < 0 then PInfinity else MInfinity

    abs (ValueP x) = ValueP (abs x)
    abs _ = PInfinity

    fromInteger x = ValueP x

    signum MInfinity = ValueP (-1)
    signum PInfinity = ValueP 1
    signum (ValueP x) = ValueP (signum x)
-- +, -, *, negate 


-- 4
type Dict = [(String, Integer)]
type Var = String

-- 1
valueOf :: String -> Dict -> Integer
valueOf key [(k, v)] = v
valueOf key ((k, v) : xs) = if key == k then v else valueOf key xs

-- 2
ins :: String -> Integer -> Dict -> Dict
ins key val [] = [(key, val)]
ins key val ((k, v) : xs) = if key == k
                            then (key, val) : xs
                            else (k, v) : (ins key val xs)

data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 
 
eval_pexpr :: Dict -> PExpr -> Integer
eval_pexpr dict (Var x) = valueOf x dict
eval_pexpr dict (Val x) = x
eval_pexpr dict (e1 :+: e2) =  (eval_pexpr dict e1) + (eval_pexpr dict e2)
 
data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 
 
eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr dict (x :==: y) = (eval_pexpr dict x) == (eval_pexpr dict y)
eval_bexpr dict (x :<: y) = (eval_pexpr dict x) < (eval_pexpr dict y)
eval_bexpr dict (Not x) = not (eval_bexpr dict x)
eval_bexpr dict (x :&&: y) = (eval_bexpr dict x) && (eval_bexpr dict y)
 
data Prog = PlusPlus Var |       
            Var :=: PExpr |     
            DeclareInt Var |     
            Begin Prog Prog |     
            While BExpr Prog |     
            If BExpr Prog Prog      
  
eval_prog :: Dict -> Prog -> Dict
eval_prog dict (DeclareInt x) = ins x 0 dict
eval_prog dict (PlusPlus x) = ins x ((valueOf x dict) + 1) dict
eval_prog dict (x :=: ((Var y) :+: (Val v))) = ins x ((valueOf y dict) + v) dict
eval_prog dict (While cond p) = eval_prog dict p
eval_prog dict (If cond p p') = eval_prog (eval_prog dict p) p'
eval_prog dict (Begin p p') = eval_prog (eval_prog dict p) p'  

class Eval a b where
    eval :: Dict -> a -> b

instance Eval PExpr Integer where
    eval = eval_pexpr

instance Eval BExpr Bool where
    eval = eval_bexpr

instance Eval Prog Dict where
    eval = eval_prog