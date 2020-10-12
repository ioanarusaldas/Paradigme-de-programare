--Ex1
factorial n = n:(map(*n) (factorial(n+1)))

--Ex2
my_seq = map(\x->1/x) (factorial 1)

--Ex3
p_sum(h:t) = h:(map(+h)) (p_sum t))

--Ex4
val_e = map (+1) (p_sum my_seq)

--Ex5
take_tol e (x:y:xs) 
	|abs(x - y) > e = take_tol (y : xs)
	|otherwise = x

--Ex6 
multi_call f a = a : map(f (multi_call f a))

--Ex7
sqrt_aprox n = multi_call(\x->(x+n/x)/2) 10


get_aprox n = take_tol 0.0001 (sqrt_aprox n)