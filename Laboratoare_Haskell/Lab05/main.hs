--Ex1. Define the type list with elements of type Integer:
data IntList = Empty | Cons Integer IntList


--Ex2. Define a function which computes the sum of elements of such a list:
isum :: IntList -> Integer
isum Empty = 0
isum (Cons h t) = h + (isum t)

--Ex3. Define type polymorfic type List a encoding lists with elements of type a:
data List a = Nil | Const a (List a) deriving Show

--Ex4. Define a function which converts IntList lists to List Integer lists:
to_poly_list :: IntList -> List Integer
to_poly_list Empty = Nil
to_poly_list (Cons h t) = Const h (to_poly_list t)

--Ex5. Define a function which displays lists. What type will this function have?
show_list Nil = "[]"
show_list (Const h t) = show h ++ ":"++show_list t
--Add the tree datatype definition from the lecture:

data Tree a = Void | Node (Tree a) a (Tree a) deriving Show

--Ex6. Implement the function flatten:
flatten :: Tree a -> List a
flatten Void = Nil
flatten (Node l k r) = app (flatten l) (Const k (flatten r))

--Ex7. Define list concatenation over type List a:
app :: (List a) -> (List a) -> (List a)
app Nil Nil = Nil
app Nil l@(Const h t) = l
app l@(Const h t) l2 =  Const h (app t l2)

--Ex8. Define the function tmap which is the Tree a correspondent to map::(a→b) → [a] → [b].
tmap f Void = Void
tmap f (Node l k r) = Node (tmap f l) (f k) (tmap f r)  

--Ex9. Define the function tzipWith:
tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f Void t2 = Void
tzipWith f (Node l1 k1 r1) (Node l2 k2 r2) = (Node (tzipWith f l1 l2) (k1 `f` k2) (tzipWith f r1 r2))

test = tzipWith (-) (Node (Node (Node Void 1 Void) 2 (Node Void 4 Void)) 3 (Node Void 0 Void)) (Node (Node (Node Void 1 Void) 2 (Node Void 4 Void)) 3 (Node Void 0 Void))

--Ex10. Define the function tfoldr:
--Before implementing it, consider what should tfoldr (+) 0 do, and how should this value be computed.
--myfoldr op acc [] = acc
--myfoldr op acc (x:xs) = op x (myfoldr op acc  xs)
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f acc Void = acc
tfoldr f acc (Node l k r) = tfoldr f (k `f`(tfoldr f acc r)) l 

test2 = tfoldr (+) 0 (Node (Node (Node Void 1 Void) 2 (Node Void 4 Void)) 3 (Node Void 0 Void))



--Ex11. Implement the flattening function using tfoldr:
tflatten Void = Nil
tflatten (Node l k r) = app (tflatten l) (Const k (tflatten r))

test3 = tflatten (Node (Node (Node Void 1 Void) 2 (Node Void 4 Void)) 3 (Node Void 0 Void))

tflatten1 Void = []
tflatten1 t = tfoldr (:) [] t

tflatten2 Void = Nil
tflatten2 t = tfoldr fun Nil t
				where
					fun e acc = app (Const e Nil) acc

test4 = tflatten2 (Node (Node (Node Void 1 Void) 2 (Node Void 4 Void)) 3 (Node Void 0 Void))
     
--Ex12. Consider the following definition of natural numbers extended with the value Infinity:
data Extended = Infinity | Value Integer deriving Show
--Define a function which computes the sum of two Extended values:

extSum :: Extended -> Extended -> Extended
extSum Infinity Infinity = Infinity
extSum _ Infinity = Infinity
extSum Infinity _ = Infinity
extSum (Value v) (Value v') = Value(v + v')
--(Question: can you implement it with a single case expression?)

--Ex13. Define a function which computes the equality of two Extended values:
equal :: Extended -> Extended -> Bool
equal _ Infinity = False
equal Infinity _ = False
equal (Value v) (Value v') = v == v'


--data Maybe a = Nothing | Just a
{-
which is part of the Haskell default library is useful for error handling. For instance, if a computation 
fails, a function may return Nothing. If the computation returns a result x of type a, then the function will return a value Just x.
-}
--Ex14. Implement the function lhead which behaves like head but returns Nothing if the argument of the function is the empty list:
lhead :: List a -> Maybe a
lhead Nil = Nothing
lhead (Const h t) = Just h

--Ex15. Implement the function ltail
ltail Nil = Nothing
ltail (Const h Nil) = ltail Nil
ltail (Const k (Const h t)) = Just (Const h t)
