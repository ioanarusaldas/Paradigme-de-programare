student(gigel).
student(ionel).
student(john).
student(mary).

nat(zero).
nat(X) :- X = succ(Y), nat(Y). 

% X = succ(Y) ^ nat(Y) => nat(X)
% X -> succ(zero)

% X = succ(succ(zero))
% Y = succ(zero)
% 3 + 8 -> 1 + (2 + 8) -> 1 + (1 + (1 + 8))
% -> 1 + (1 + (1 + (0 + 8)))

% add(zero, Y, Y).
add(zero, Y, E) :- E = Y.
add(X, Y, E) :- X = succ(P), add(P, Y, R), E = succ(R),!.

% minus(7, 5) -> minus(6,4) -> minus(5,3) -> minus(4,2) ->
% minus(3, 1) -> minus(2, 0)

minus(X, zero, R) :- R = X.
minus(X, Y, R) :- X=succ(M), Y=succ(N), minus(M, N, R1), R = R1, !.

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

min(zero, _, R) :- R = zero.
min(_, zero, R) :- R = zero.
min(X, Y, R) :- X=succ(M), Y=succ(N), min(M, N, R1), R = succ(R1), !.

%Ex4
max(zero, N, R) :- R = N.
max(N, zero, R) :- R = N.
max(X, Y, R) :- X=succ(M), Y=succ(N), max(M, N, R1), R = succ(R1), !.

%Ex5
grater(succ(X), zero) :- nat(X).
grater(succ(X), succ(Y)) :- grater(X, Y).