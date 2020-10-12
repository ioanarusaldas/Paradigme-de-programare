nat(zero).
nat(X) :- X = succ(Y), nat(Y).

% 1
add(zero, Y, Y) :- nat(Y).
add(succ(X), Y, Rez) :- add(X, succ(Y), Rez). 

% 2
minus(X, zero, X) :- nat(X).
minus(succ(X), succ(Y), Rez) :- minus(X, Y, Rez).

%%% 

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

% 3
min(X,zero,zero) :- nat(X).
min(zero,Y,zero) :- nat(Y).
min(succ(X),succ(Y),succ(Rez)) :- min(X,Y,Rez). 


% 4
max(X,zero,X) :- nat(X).
max(zero,Y,Y) :- nat(Y).
max(succ(X),succ(Y),succ(Rez)) :- max(X,Y,Rez).


% 5
gt(succ(X),zero) :- nat(X).
gt(succ(X),succ(Y)) :- gt(X,Y).

% 6
leq(X,Y):- \+gt(X, Y).

% 7
div(X, zero, zero) :- nat(X).
div(X, Y, zero) :- gt(Y, X).
div(X, Y, R) :- div(Aux, Y, R1), minus(X, Y, Aux), add(R1, succ(zero), R).

% 8
mod(X, zero, zero) :- nat(X).
mod(X, Y, X) :- gt(Y, X).
mod(X, Y, R) :- mod(Aux, Y, R), minus(X, Y, Aux).

% 9
gcd(X, X, X) :- nat(X).
gcd(X, Y, R) :- gt(X, Y), minus(X, Y, Aux), gcd(Aux, Y, R).
gcd(X, Y, R) :- gt(Y, X), minus(Y, X, Aux), gcd(X, Aux, R).


%%% Liste

isList(void).
isList(cons(_,T)) :- isList(T).

% 10
head(void, void).
head(cons(H,T), H) :- isList(T).

% 11
tail(void, void).
tail(cons(_,T), T) :- isList(T).

% 12
concat(void, X, X).
concat(cons(H1, T1), L2, cons(H1, R)) :- concat(T1, L2, R).

% 13
% rev_aux/3
rev_aux(void, X, X).
rev_aux(cons(H, T), X, R) :- rev_aux(T, cons(H, X), R).
% reverse/2
reverse(L, R) :- rev_aux(L, void, R).

%%%

fromList(void,[]).
fromList(cons(H,T),[H|R]) :- fromList(T,R).

% 14
toList([], void).
toList([H|R], cons(H,T)) :- toList(R, T).


% 15
kelem([H|_], 1, H).
kelem([_|T], K, R) :- kelem(T, KAux, R), K is KAux+1. 

% 16
rem([], []).
rem([X], [X]).
rem([H1, H2|T], [H2|R]) :- rem([H2|T], [H2|R]), H1=H2.
rem([H1, H2|T], [H1, H2|R]) :- rem([H2|T], [H2|R]), H1\=H2.


% 17
flatten([], []).
flatten([H|T], R) :- flatten(H, R1), flatten(T, R2), append(R1, R2, R).
flatten(L, [L]).

%%%% - de inteles cum functioneaza
%18
pack([H|[]], [[H]]).
pack([H|T], [[H | [H|T1]] | T2]) :- pack(T, [[H|T1]|T2]).
pack([H|T], [ [H] | [[H1|T1]|T2] ]) :- pack(T, [[H1|T1]|T2]), H \= H1.
%%%%


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