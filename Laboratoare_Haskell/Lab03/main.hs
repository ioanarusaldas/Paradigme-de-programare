--43. Define the operator ($) from Lab 2 as a higher-order function.
--($) :: (a -> b) -> a -> b
-- The $ operator is for avoiding parentheses.
--Anything appearing after it will take precedence over anything that comes before.

--44. What type should functional composition have?
----(.) :: (b -> c) -> (a -> b) -> a -> c

--45. Define function composition.
--myComp f g = f $ g

--Lambdas
--46. Functions can be passed as arguments just like any other value value. 
--Also, functions can be returned as parameter. In order to do so, it is convenient 
--to define functions without naming them. This is done using lambda's. 
--For a more detailed discussion regarding lambdas, see the lecture. The following definitions are equivalent:

{-f x y = x + y 
f x = \y -> x + y
f = \x -> \y -> x + y
f = \x y -> x + y
That is the type of f? What is the type of f 5?-}
--f :: Num a => a -> a -> a
--f 5 :: Num a => a -> a

--47. Consider sets represented as characteristic functions with signature s :: Integer → Bool, where s x is true if x 
--a member in the set. Examples:

s1 1 = True
s1 2 = True
s1 _ = False
s2 x = if x `mod` 2 == 0 then True else False
s3 2 = True
s3 _ = False
--Above, s1 is the set {1,2}, s2 is the set of even integers and s3 is the empty-set. 
--Write a function which tests if an element is a member of a set:
mem :: (Integer -> Bool) -> Integer -> Bool
mem s x = s x

--Ex48. Define the set {2^n∣n∈N}.
s4 x = f x 
		where
			f x
				| x == 1 = True
				| x < 1 = False
				|otherwise = f (x / 2)



--Ex49. Define the set of natural numbers.
isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)
s5 x = isInt x && x >= 0 

--Ex50. Implement the intersection of two sets. Use lambdas.
--intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection = \s1 s2 x -> (s1 x) && (s2 x)

--Ex51. Write intersection in another way, (without using lambdas).
--intersection' :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersection' s1 s2 x = (s1 x) && (s2 x)

--Ex52. Write a function which takes a list of integers, and returns the set which contains them.
toSet :: [Integer] -> (Integer -> Bool)
toSet l x = elem x l

--53. Implement a function which takes a list of sets and computes their intersection.
capList :: [Integer -> Bool] -> (Integer -> Bool) 
capList (x:y:[]) n = (intersection x y n)
capList (x:xs) n = (x n) && (capList xs n)

--Filter
--Ex54. Write a function which receives a predicate p :: Integer → Bool, a list of integers l, 
--and returns a list of integers from l for which g is true. Your implementation is the filter function from Haskell.
myfilter g [] = []
myfilter g (x:xs) 
	| g x == True = x : myfilter g xs
	|otherwise = myfilter g xs

--Ex55. Solve exercise 24. from Lab 2 using filter.
{- Lab02
f24 [] = []
f24 (x:xs)
	| x >= 0 = x : (f24 xs)
	|otherwise = f24 xs-}
f55 l = filter (>=0) l

--Ex56. Test the function map::(a→b) → [a] → [b] from Haskell. Implement it.
myMap g [] = []
myMap g  (x:xs) = (g x) : (myMap g xs)

--Ex57. Solve exercise 17. from Lab 1 using map.
f57 l = map aplica l
		where
			aplica x 
				|x == True = 1
				|otherwise = 0

--Ex58. Solve exercise 27. from Lab 2 using map and filter. 
--(Hint. Pattern matching can be used in lambdas. Use fst and snd to introspect pairs.)
{-f27 _ [] = []
f27 grupa ((s,d) : xs)
	|s == grupa = (extract d) ++ (f27 grupa xs)
	|otherwise = (f27 grupa xs)
	where
		extract [] = []
		extract (y:ys)
			|(head y) == 'M' = y : extract ys
			|otherwise = extract ys
-}
-- “321CB” [(“321CB”, [“Matei”, “Andrei”, “Mihai”]), (“322CB”,[“George, Matei”])] = [“Matei”, “Mihai”]
f58 grupa l@((s,d) : xs) = head $ map select (filter grup l)
							where
								grup (s,d) = s == grupa
								select(s,d) = filter findM d
												where
													findM x = (head x) == 'M'
								
									


--Ex59. Solve exercise 31. from Lab 2 using map and filter.
  --f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] = [(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
f59 l = map nume (filter nota l)
 			where 
  				nota (s,d) = d >= 5 
  				nume (s,d) 
  					| length (words s) == 3 = ((words s),d + 1)
  					|otherwise = ((words s),d)


--Folds
--Ex60. Write a function which appends a list of lists of integers:
app :: [[Integer]] -> [Integer] 
app l = foldl (++) [] l

--Ex61. Write the same function tail-recursively. Are the two functions identical? Why?
app2 [] = [] 
app2 (x:xs) = x ++ app2 xs

--Ex62. Implement foldr (see lecture).
myfoldr op acc [] = acc
myfoldr op acc (x:xs) = op x (myfoldr op acc  xs)

--Ex63. Implement foldl (see lecture).
myfoldl op acc [] = acc
myfoldl op acc (x:xs) = (myfoldl op (acc `op` x) xs)

--Ex64. Implement list concatenation using a fold.
--f64 l1 l2 = foldr (:) l1 l2
f64 l = foldr (++) [] l


--Ex65. Implement list reversal using a fold.
f65 l = foldl (\acc x-> x : acc) [] l


--Ex66. Implement the function from exercise 52 using folds. (Hint: thing about what the accumulator should hold)
--EX52
--toSet :: [Integer] -> (Integer -> Bool)
--toSet l x = elem x l
f66 l n = foldl op False l
			where
				op acc x = acc || (x == n)
--Ex67. Implement exercise 53 using a fold.
f67 l n = foldl op True l
				where
					op acc x =  acc && (x n)

f68 l n = foldl (+) a b
				where
					a = head l
					b = tail l
