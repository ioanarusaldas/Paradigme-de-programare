import Data.List
--Ex1. Write a constant function in Haskell.
f1 = 5

--Ex2. Write the Haskell implementation of the function below: 

{-f(x,y) = x (the Ox projection function)

f :: Integer -> Integer -> Integer
f x y = x + y

f :: Bool -> Bool
f True = False
f False = True-}

f2 x y = x

--Ex3. Write a function together with its signature, which implements boolean AND:
myand :: Bool -> Bool -> Bool
myand x y 
	|(x == True) && (y == True) = True
	|otherwise = False

--Ex4. What is the signature of the solutions for exercises 1. 2. ? Use :t to find out. What does a mean?
--f1 :: Integer
--f2 :: t1 -> t -> t1

--Ex5. Write an implementation for:
--if' :: Bool -> a -> a -> a
--How many parameters does if' take?
--What is the interpretation of each parameter?
--What should if' return?
f5 b x y 
	|b == True = x
	|otherwise = y

--Ex6. Write a function which takes three integers and returns the largest. Hint - sometimes parentheses are useful.
f6 :: Integer -> Integer -> Integer -> Integer
f6 x y z 
	|x >= y && x > z = x
	|y >= z && y > x = y
	|otherwise = z

--Ex7. What is the type of the function f defined below:
f7 x y z = if x then y else z
--f7 :: Bool -> t -> t -> t

--Ex8. What is the type of:
  -- f x y z 
   	 -- | x == True = y
   	 -- | otherwise = z
--Ex9. Solve exercise 6. in a different way. (&& may be helpful in boolean conditions).
--la fel ca 6, nu stiu altfel

--Ex10. What is the type of [1,2,3] ?
--[1,2,3] :: Num t => [t]

--Is 1:[2,3] the same thing as 1:2:3:[] ?
	--da
--Is 1:(2:[]) the same thing as (1:2):[] ?
	-- da

--Ex11. Solve exercise 6 in a different way. (the function sort may be helpful, as well as the functions head, tail and reverse. Use :t on each one, and test them).
f11 l = head (reverse (sort l))
--Ex12. Implement a list reversal function.
myreverse [] = []
myreverse l = (last l ) : myreverse (init l)

--Ex13. Write a function which extracts the third to last number from a list and returns True, if that number is odd (hint: the function mod may be useful)
  -- f [3,4,5,2,3,9] = False 
  -- f [3,4,2,1,4,4] = True
f13 l
	|length l < 3 = False
f13 l = odd (last(init (init l) ))

--Ex14. Implement a function which returns the sum of integers from a list.
f14 l = foldl (+) 0 l

--Ex15. Implement a function which takes a list of booleans and returns false if at least one boolean from the list is false.
f15 l 
	|length(filter (\x -> x == False) l) == 0 = True
	|otherwise = False

--boolList :: [Bool] -> Bool
--boolList [] = True
--boolList (e:l) = myand e (boolList l) 

--Ex16. Implement a function which filters out all odd numbers from a list.
f16 l = filter odd l

--Ex17. Implement a function which takes a list of booleans and returns a list of integers. In the latter, (True becomes 1 and False becomes 0). Example: f [False, True, False] = [0,1,0].
f17 l = map aplica l
		where
			aplica x 
				|x == True = 1
				|otherwise = 0
{-Ex18. Sometimes it is helpful to define auxiliary functions which are not used later in our program. For instance:

   f :: [[Integer]] -> [Bool]
	f [] = []
	f l = (g (head l)):(f (tail l))
		where
			g [] = True
			g l = h (tail l)
			h [] = True
			h l = False
What does the previous function do? Test it.
-}


--Ex19. Implement a function which takes a list of booleans and returns the sum of True values from the list. Example f [False,True,False,False,True] = 2.
--f19 l= length(filter (\x -> x == True) l) 
f19 l = foldl (+) 0 (f17 l)

--Ex20. Implement insertion sort.
ins elem [] = [elem]
ins elem (y:ys)
 	|elem < y = elem : (y:ys)
 	|otherwise = y : (ins elem ys)

myinssort :: [Integer] -> [Integer]
myinssort [] = []
myinssort (e:l) = ins e (myinssort l)
