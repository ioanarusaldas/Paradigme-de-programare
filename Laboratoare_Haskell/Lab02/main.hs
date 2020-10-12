import Data.List
import Data.Word
--Ex21. Extract the fourth element from a list of integers
f21:: [Integer]-> Integer -> Integer
f21 [] _ = 0
f21 (x:xs) nr 
	|nr == 4 = x
	|otherwise = (f21 xs (nr + 1))

--Ex22. Write the same function which returns 0 if the list contains less that four elements and 1 otherwise.
f22 l 
	|length l < 4 = 0
	|otherwise = 1

--23. What are the types of x,y,z,w and that of l in the implementation below?
--f23 :: Num t => [t] -> t
--f23 (x:y:z:w:l) = w
--f23 _ = 0

--f23 :: [t] -> t
--f23 (x:y:z:w:l) = w
--What is the difference?

--Ex24. Write a function which filters out all negative numbers from a list. Use patterns.
f24 [] = []
f24 (x:xs)
	| x >= 0 = x : (f24 xs)
	|otherwise = f24 xs


--Ex25. What is the type of the function:
--f25 :: t1 -> t -> (t1, t)
f25 x y = (x,y)
--In the body of a function definition, , is a base constructor for pairs. The following are pairs: (1,2), (“Matei”,20), ([1,2],3). 
--Note that there is no restriction on the type of the first and second values of a pair.

--Ex26. What is the type of the function:
--f26 :: Char->[Char]->[Char]
--f26 'a' _ = []
--f26 x y = x:y
--In Haskell, the type String is equivalent to [Char] (hence strings are lists of chars). 
--Strings can be introspected using patterns just like any other list.

--Ex27. Let f “321CB” [(“321CB”, [“Matei”, “Andrei”, “Mihai”]), (“322CB”,[“George, Matei”])] = [“Matei”, “Mihai”]
--What is the signature of f?
--What does f do (note that Matei and Mihai both start with letter 'M')?
--Write an implementation for f.
f27 _ [] = []
f27 grupa ((s,d) : xs)
	|s == grupa = (extract d) ++ (f27 grupa xs)
	|otherwise = (f27 grupa xs)
	where
		extract [] = []
		extract (y:ys)
			|(head y) == 'M' = y : extract ys
			|otherwise = extract ys


--Ex28. Write a function which returns True if the third largest element from a list is positive
third l
	|length l < 3 = -1
third l = last(init (init l))

f28 l 
	|third (sort l) >= 0 = True
	|otherwise = False

--Ex29. Sometimes it really helps to define function via function composition. For instance:

f29 x = (g.h) x
	where g x = 2*x
	      h x = x + 1
--What type does f have? What type does g have?
--f29 :: Num c => c -> c


--What does the following function do?
--f x = ff x
--	where g x = 2*x
--	      h x = x + 1
--	      ff = f.h

--What does the following function do?
--f x = ff x
	--where g x = 2*x
	--      h x = x + 1
	--      ff = h.f

--Ex30. Rewrite exercise 28 via function composition:
f30 l = (f.third.h) l
	where 
		h l = sort l
		f x = if (x >= 0) then True else False 
--Ex31. Write a function which takes a list of pairs - student-name and grade, removes those with grades less than 5, 
--splits the name in substrings, 
--and adds one bonus point to people with three names. 
--Example:
    --f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] = [(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
f31 l =  (h.g) l
	where
		g [] = []
		g ((s,d) : xs)
			|d > 5 = (s,d) : g xs
			|otherwise = g xs
		h [] = []	
		h ((s,d) : xs) 
			| length (words s) == 3 = ((words s),(d + 1)) : (h xs)
			|otherwise =((words s),d) : (h xs)
	

--Ex32. Write the signature and implement the following function:
  -- f ["Matei", "Mihai"] ["Popovici","Dumitru"] = [("Matei","Popovici"), ("Mihai","Dumitru")]
--f32 l1 l2 = zipWith (++) l1 l2
f32 [] [] = []
f32 [] _ = []
f32 _ [] = []
f32 (x:xs) (y:ys) = (x,y) : (f32 xs ys)
--Ex33. Implement the following function:
  -- f ["Matei", "Mihai"] ["Popovici","Dumitru"] = ["MPopovici", "MDumitru"]
f33 [] [] = []
f33 [] _ = []
f33 _ [] = []
f33 (x:xs) (y:ys) = ((head x):y) : (f33 xs ys)

--Ex34. Sometimes it helps to use functions in infix form. For instance, instead of mod x 2, 
--we can write x mod 2. We can also define infix functions:
	--x`mod`y = x + y
--Implement a function for transforming lists into pairs, in exercise 32. and use it in the infix form.
f34 l1 l2 = l1 `f32` l2

{-35. Just in the same way, we can treat infix functions as prefix. We do this using round parentheses. Test:
		:t (+) (+) :: Num a => a -> a -> a
		:t (&&) (&&) :: Bool -> Bool -> Bool
		:t (++)-} --(++) :: [a] -> [a] -> [a]


--36. What about function composition? Is it a special operator, or is it just a function as well? What does :t (.) do?
--(.) :: (b -> c) -> (a -> b) -> a -> c


--37. What does the function below do? Take parts of the function below, and test them.
f37 l = (((++) "?").((:) '>')) l 
--f37 "ana"
--"?>ana"
f37' = (((:) '>').(++"?"))
--f37' "ana"
--">ana?"

--Ex38. Write a function of a single line, such that:
--f "Matei" = "{Matei}"
f38 l = "{" ++ l ++ "}"

--Ex39. Use only (:), reverse and functional composition . to solve exercise 38
f39 l = (((:) '{').reverse.((:) '}').reverse) l

--Ex40. What does the function ($) to?
--($) :: (a -> b) -> a -> b
 -- (Use :t, and make up some tests)
--Ex41. Write the following implementation using only ($):
--f "||" "Matei" = "||Matei||"
f41 x y = (++) x $ (++) y x
--Ex42. Solve exercise 13. from Lab 2 using $ (++ and reverse are also permitted) 
--and other improvements (treat the case when the list has fewer elements):
{-Lab2
f13 l
	|length l < 3 = False
f13 l = odd (last(init (init l) ))-}
f42 l
	|length l < 3 = False
f42 l = odd $ last $ init $ init l