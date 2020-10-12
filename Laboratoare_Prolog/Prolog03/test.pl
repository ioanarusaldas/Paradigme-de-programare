%swipl
%Ex1
app([],L,L).
app([H|T],L,[H|Rp]) :- app(T,L,Rp).


sublist(_,[]).
sublist(L,[H|T]) :- app(R1,_,L),app(_,[H|T],R1). 

generate(_,[]).
generate(X,[X|R]) :-Y is X + 1, generate(Y,R).

natList(R) :- generate(0,R).

oddOnly([],[]).
oddOnly([H|T],[H|R]) :- H mod 2 =:= 0, oddOnly(T,R).
oddOnly([_|T],R) :- oddOnly(T,R).

oddAuxList(_,[]).
oddAuxList(X,[X|R]) :-Y is X + 2, oddAuxList(Y,R).

oddList(R) :- oddAuxList(2,R).

%eqelem(L), length(L,3), L=[0|_].

aux(_,[]).
aux(X,[H|T]) :- H = X,aux(X,T).
%eqelem(L) :- aux(_,L).


eqelem([]).
eqelem([H,H|T]) :- eqelem([H|T]).

%repeat(_,_,[]).

repeat(L,K,[L|T]) :- eqelem([L|T]),length([L|T], K).
