/* :- violations(K)*/

violations(K) :-
	places(P),
	findall([X,Y,Z], trans(X,Y,Z), T),
	range(1, P, L),
	setof(S, violations(L, T, S), [K|_]).
	
violations(P, T, S) :-
	perm(P, L),
	computeViolations(L, T, S).
	
computeViolations(_, [], 0).
computeViolations(L, [[_, Y, Z] |T], S) :-
	computeViolations(L, T, S1),
	isViolation(L, Y, Z, Ans),
	S is S1 + Ans.
	
isViolation([Y|_], Y, _, 0) :- !.
isViolation([Z|_], _, Z, 1) :- !.
isViolation([_|T], Y, Z, Ans) :-
	isViolation(T, Y, Z, Ans).
	
	
remove(X, [X|L], L).
remove(X, [Y|L], [Y|L1]) :-
	X \= Y,
	remove(X, L, L1).
	
perm([],[]).
perm([H|T], L) :-
	perm(T, Z),
	remove(H, L, Z).
	
trans(X, Y, Z) :-
	order(X, Y, Z).
	
trans(X, Y, Z ) :-
	order(X, Y, W),
	trans(X, W, Z).
	
/* range - Gives I, J returns [I, I+1, I+2,.., J]
 *
 * Parameters :
 * 		(I, J, List).
 */
range(I, I, [I]).
range(I,J, [I|T]) :-
	I =< J,
	I1 is I+1,
	range(I1, J, T).