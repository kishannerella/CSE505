/*
 * Example - resolution('E:\\Logic\\Assignments\\ass2\\input').
 */

resolution(InputFile):-
	load_dyn(InputFile),
	top.

member(H, [H|_]).
member(H, [_|T]):-
	member(H, T).

append([],L,L).
append([H|T1], L, [H|T2]):-
	append(T1, L, T2).

delete(_, [], []).
delete(H, [H|T], T).
delete(H, [X|T], [X|T1]) :-
	H \= X,
	delete(H, T, T1).
	

removeDuplicates([], []).
removeDuplicates([H|T], NewL):-
	member(H, T),
	removeDuplicates(T, NewL).
	
removeDuplicates([H|T], [H|NewL]) :-
	\+ member(H, T),
	removeDuplicates(T, NewL).


negDisjunction(N, or(X,Y), S) :-
	negDisjunction(N, X, S1),
	negDisjunction(N, Y, S2),
	append(S1, S2, S).
	
negDisjunction(N, neg(X), [(N, [X])]).
negDisjunction(N, X, [(N, [neg(X)])]) :-
	X \= or(_,_),
	X \= neg(_).

	
readInput(N, NQ):-
	myQuery(N, Q),
	negDisjunction(N, Q, NQ).

readInput(N, S):-
	N1 is N+1,
	myClause(N, X),
	expand(X, L),
	readInput(N1, S1),
	append([(N, L)], S1, S).
	
expand(or(X, Y), L) :-
	expand(X, L1),
	expand(Y, L2),
	append(L1, L2, L).

expand(X, [X]) :- 
	X \= or(_,_).
	
top() :-
	readInput(1, S),
	res(S).
	
res(S) :-
	myQuery(Num,_),
	Num1 is Num +1,
	resolve(S, [], Num1).

hasComplement(X, Y, Z):-
	member(X1, X),
	member(Y1, Y),
	((neg(X1) = Y1, Z = X1)
	; (X1 = neg(Y1), Z = Y1)).

isTautology(C) :-
	member(X, C),
	hasComplement([X], C, _).

addRule(N1, N2, N3, C, L, NewL) :-
	append(L, [(N1, N2, C, N3)], NewL).
%write(NewL),nl.
	
subset([], _).
subset([H|T], L) :-
	member(H,L),
	subset(T,L).
	
equalRules(R1, R2) :-
	subset(R1, R2),
	subset(R2, R1).
	

duplicateRule(C, L) :-
	member((_, X), L),
	equalRules(C, X).
	
formOr(empty, empty).
formOr([X], X).
formOr([H1,H2|T], or(H1,L)):-
	formOr([H2|T], L).
	
printRules([]).
printRules([H|T]) :-
	H = (N1, N2, C, N3),
	formOr(C, L),
	writeln(resolution(N1, N2, L, N3)),
	printRules(T).
	
%ResL is of the form [(1,2,[a,b,c], 4), (5,6,[a,b], 7)]
%S is of the form [(4,[a,b,c]), (7,[a,b])]
resolve(S, ResL, Num) :-
	(member(X, S),
	member(Y, S),
	X \= Y,
	X = (N1, C1),
	Y = (N2, C2),  
	hasComplement(C1, C2, Lit),
	delete(Lit, C1, C3),
	delete(neg(Lit), C3, C5),
	delete(Lit, C2, C4),
	delete(neg(Lit), C4, C6),
	((C5 = [], C6 = [])
	 -> addRule(N1, N2, Num, empty, ResL, ResL1),
		printRules(ResL1),
		writeln(resolution(success))
	 ; append(C5, C6, C7),
	   removeDuplicates(C7, C8),
	   \+ isTautology(C8),
	   \+ duplicateRule(C8, S),
	   addRule(N1, N2, Num, C8, ResL, NewResL),
	   append(S, [(Num, C8)], NewS),
	   Num1 is Num+1,
	   resolve(NewS, NewResL, Num1)));
	writeln(resolution(fail)).
	