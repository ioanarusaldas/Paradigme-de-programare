
nat(zero).
nat(X) :- X = succ(Y), nat(Y).


%1. Implement the predicate add/3 which adds two natural numbers.
%adun x cu y si mai adaug un succ

add(zero,Y,Y).
add(succ(X),Y,Res) :- add(X,Y,Z), Res = succ(Z). 

%2. Implement the predicate minus/3 which substracts one natural number from another. 
%Substraction only needs to work as long as the result is a natural number.
%Consider the following conversion predicates between naturals and Prolog integers.

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

minus(X,zero,X).
minus(succ(X),succ(Y),Res) :- minus(X,Y,Res).


%3. Implement min/3 which computes the minimum of two natural numbers. Example usage:

%min(X,zero,zero).
%min(zero,Y,zero).
%min(succ(X),succ(Y),R) :- min(X,Y,Rp), R = succ(Rp).


min(_,zero,zero).
min(X,Y,Y) :- minus(X,Y,_),!.
min(X,_,X).

%4. Implement max/3 which computes the maximum of two natural numbers.

%max(X,zero,X) :- nat(X).
%max(zero,Y,Y) :- nat(Y).
%max(succ(X),succ(Y),succ(Rez)) :- max(X,Y,Rez).

max(_,zero,zero).
max(X,_,X) :- minus(X,_,_),!.
max(_,Y,Y).

/*5. Implement gt/2 which is satisfied if the first natural number is strictly greater than the second.*/

gt(succ(_),zero).
gt(succ(X),succ(Y)) :- gt(X,Y).


%6. Implement leq/2 which is satisfied if the first natural number is less than or equal to the second.

lt(zero,_).
lt(succ(X),succ(Y)) :- lt(X,Y).

%7. Implement div/3 which implements the div operator.
%div(X, zero, zero) :- nat(X).
%div(X, Y, zero) :- gt(Y, X).
%div(X, Y, R) :- div(Aux, Y, R1), minus(X, Y, Aux), add(R1, succ(zero), R).


div(X,Y,0) :- gt(Y,X).
div(X,Y,R) :- gt(Y,zero),minus(X,Y,Rez),div(Rez,Y,Rp),R is Rp + 1.


%8. Implement mod/3.
mod(X,Y,X) :- gt(Y,X).
mod(X,Y,R) :- gt(Y,zero),minus(X,Y,Rez),mod(Rez,Y,R).


%9. Implement gcd/3 which computes the greatest common divisor of two natural numbers.
%Consider the following representation of lists, expressed by the predicate isList shown below.
gcd(X,Y,X) :- lt(Y,zero).
gcd(X,Y,R) :- gt(X,Y),mod(X,Y,Rez),gcd(Y,Rez,R).
gcd(X,Y,R) :- gt(Y,X),mod(Y,X,Rez),gcd(X,Rez,R).

isList(void).
isList(cons(_,T)) :- isList(T).

%10. Implement head/2 and tail/2.

head(cons(H,_),H).
tail(cons(_,T),T).

%11. Implement size/2 which determines the length of a list as a Prolog integer.

size(void,0).
size(cons(_,T),R) :- size(T,Rp), R is Rp +1.

%12. Implement concat/3 which concatenates two lists.

concat(void,L,L).
concat(cons(H,T),L,R) :- concat(T,L,Rp), R = cons(H,Rp).

/*13. Implement reverse/2 which reverses a list Use accumulators instead of concatenation and an auxiliary predicate.*/

%reverse(cons(H,void),H).
%reverse(cons(H,T),R) :- reverse(T,R), R = cons(H,R). 

% rev_aux/3
rev_aux(void, X, X).
rev_aux(cons(H, T), X, R) :- rev_aux(T, cons(H, X), R).

reverse(L, R) :- rev_aux(L, void, R).



%Consider the following conversion functions from the above list representation to Prolog lists.

fromList(void,[]).
fromList(cons(H,T),[H|R]) :- fromList(T,R).

%14. Write the predicate toList/2 which converts a Prolog list to our list representation.

toList([],void).
toList([H|T],cons(H,R)) :- toList(T,R).

%15. Implement kelem/3 which determines the k-th element of a Prolog list.
kelem([H|_],0,H).
kelem([_|T],K,R) :- Kp is K - 1, kelem(T,Kp,R).


%16. Implement rem/2 which removes consecutive duplicates from a Prolog list.
%rem([1,1,1,2,2,3,4,4],R).
%R = [1,2,3,4].

%rem([],[]).
%rem([X], [X]).
%rem([H|T],[H2|_]) :-rem(T,[[H|H2]|T]), H\=H2.
%rem([H|T],[H2|_]) :-rem(T,[H|T]), H = H2.
 
rem([], []).
rem([X], [X]).
rem([H1, H2|T], [H2|R]) :- rem([H2|T], [H2|R]), H1=H2.
rem([H1, H2|T], [H1, H2|R]) :- rem([H2|T], [H2|R]), H1\=H2.

%17. Implement flatten/2 which flattens nested Prolog lists. Do not use auxiliary predicates.

%flatten([1,2,[3,4,[5]], [[6],[]], [7]], R).
%R = [1,2,3,4,5,6,7]

flatten([], []).
flatten([H|T], R) :- flatten(H, R1), flatten(T, R2), append(R1, R2, R).
flatten(L, [L]).
 
/*18. Implement pack/2 which groups consecutive duplicates into sublists.*/


pack([H|[]], [[H]]).
pack([H|T], [[H | [H|T1]] | T2]) :- pack(T, [[H|T1]|T2]).
pack([H|T], [ [H] | [[H1|T1]|T2] ]) :- pack(T, [[H1|T1]|T2]), H \= H1.

%19

take(_,0, []).
take([H|T], N, [H|Rp]) :- N > 0, Np is N-1, take(T, Np, Rp).

drop(L, 0, L).
drop([_|T], N, R) :- Np is N-1, drop(T, Np, R). 

slice(L, N1, N2, R) :- 	NAux is N1 - 1,
						drop(L, NAux, R1),
						N3 is N2-N1+1,
						take(R1, N3, R).


slice2([_|T], N1, N2, R) :- N1 > 0, N2 > 0, N1_p is N1 - 1, N2_p is N2 - 1,
								slice2(T, N1_p, N2_p, R).
slice2(_, 0, 0, []).
slice2([H|T], 0, N2, [H|R]) :- N2 > 0, N2_p is N2 - 1,
								slice2(T, 0, N2_p, R).
% 20

% rotate(List, nr, result)
rotate(L, N, L) :- length(L, N).
rotate(L, N, R) :- length(L, Len), Len < N, Diff is N - Len, rotate(L, Diff, R).
rotate(L, N, R) :- length(L, Len), Len > N, slice2(L, N, Len, R1),
					slice2(L, 0, N, R2), append(R1, R2, R).
*/
