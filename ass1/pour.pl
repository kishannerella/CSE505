/*:- split() */

split() :-
	constructList(Tray),
	vessels(V),
	people(P),
	V >= P,
	tryMovesWithDepth(Tray, 0).

tryMovesWithDepth(Tray, D) :-
	%writeln(D),
	horizon(Limit),
	D < Limit,
	D1 is D+1,
	(divide(Tray, 0, D)
	;tryMovesWithDepth(Tray, D1)).
	
constructList(L):-
	vessels(V),
	range(1, V, L1),
	createZeroList(V, L0),
	merge(L1, L0, L2),
	source(S),
	capacity(S, C),
	replace(L2, S, C, L).
	%writeln(L).

divide(Tray, Depth, Limit) :-
	Depth < Limit,
	capacity(X,_),
	capacity(Y,_),
	action(Tray, X, Y, NewTray),
	(checkTray(NewTray);
	(Depth1 is Depth + 1,
	 divide(NewTray, Depth1, Limit))).

/* checkTray - Checks whether the given tray configuration
 * is valid solution.
 *
 * Parameters :
 * 		checkTray(Tray, BeerPerPerson).
 */	
checkTray(L):-
	source(S),
	people(P),
	capacity(S, TotalBeer),
	BeerPerPerson is TotalBeer//P,
	%optim
	removeEmptyVessels(L, L1),
	assign(L1, PersonTray),
	valid(PersonTray, BeerPerPerson).
	
removeEmptyVessels([],[]).

removeEmptyVessels([[_, 0] |T1], T) :-
	removeEmptyVessels(T1,T).
	
removeEmptyVessels([[X,Y] |T1], [[X,Y] | T]) :-
	Y > 0,
	removeEmptyVessels(T1,T).
	
	
valid(Tray, BeerPerPerson):-
	valid1(Tray, 1, BeerPerPerson).
	
valid1(_, K, _) :-
	people(P),
	K > P.
	
valid1(Tray, K, BeerPerPerson) :-
	people(P),
	K =< P,
	findSum(Tray, K, BeerPerPerson),
	K1 is K+1,
	valid1(Tray, K1, BeerPerPerson).


/* findSum - Given a PersonTray[Person, BeerAmount] and a Person
 * number, find how much beer the person is assigned.
 *
 * Parameters :
 * 		findSum(PersonTray, PersonNo, TotalBeer).
 */
findSum([], _, 0).
findSum([[Id, Value]|T], Id, Sum) :-
	findSum(T, Id, Sum1),
	Sum is Sum1 + Value.
	
findSum([[X,_ ]|T], Id, Sum) :-
	X \= Id,
	findSum(T, Id, Sum).
	
	


/* assign - Given a normal Tray, return a PersonTray where
 * each vessel is assigned a person. The returned PersonTray
 * has values of form [Person, BeerAmount].
 *
 * Parameters :
 * 		(Tray, PersonTray).
 */
assign([],[]).
assign([[_, Amt]|T], [[P, Amt] | T1]) :-
	%hack, using capacity to enumerate people
	capacity(X,_),
	people(TotalP),
	TotalP >= X,
	P is X,
	assign(T, T1).

/* action - Given the Tray, src and dest, action tries
 * to move the beer from Src to Destination and returns a new tray.
 *
 * Parameters :
 * 		(Tray, Source, Destination, NewTray).
 */
action(Tray, Src, Dest, NewTray) :-
	Src \= Dest,
	beerIn(Tray, Src, SrcAmt),
	beerIn(Tray, Dest, DestAmt),
	capacity(Dest, DestCap),
	DestRemain is DestCap - DestAmt,
	DestRemain > 0,
	(SrcAmt =< DestRemain
	 -> NewDestAmt is SrcAmt + DestAmt,
	    replace(Tray, Dest, NewDestAmt, Tray1),
		replace(Tray1, Src, 0, NewTray)
	 ;  NewSrcAmt is SrcAmt - DestRemain,
		replace(Tray, Dest, DestCap, Tray1),
		replace(Tray1, Src, NewSrcAmt, NewTray)).
	
	
/* beerIn - Given the Tray and an vessel number, returns the amount of
 * beer in that vessel.
 *
 * Parameters :
 * 		(Tray, VesselNumber, Amount).
 */
beerIn([[Id, Value] |_], Id, Value).
beerIn([[X, _] | T], Id, Value) :-
	X \= Id,
	beerIn(T, Id, Value).
 

/* replace - Given a (vessel, amount) list, an index and
 * a value, replace the amount with value for the index-th
 * vessel
 * Parameters :
 * 		(List, Index, Value, Result).
 * Eg: replace([[1,4],[2,5],[3,6]], 2, 10, [[1,4],[2,10],[3,6]]).
 * 
 */		
replace([], _, _, []).
replace([[Id,_]|T], Id, Value, [[Id, Value]|T]).
replace([[X,Y]|T], Id, Value, [[X,Y]|T1]) :-
	X \= Id,
	replace(T, Id, Value, T1).

	
	

/* merge - Given list L1, L2, kth element of L2 gets
 * appended to kth element of L1.
 *
 * Parameters :
 * 		(L1, L2, MergedL).
 * Eg: merge([1,2,3], [4,5,6], [[1,4],[2,5],[3,6]]).
 * 
 */		
merge([],[], []).
merge([H1|T1], [H2|T2], [[H1,H2] | T]) :-
	merge(T1, T2, T).

/* createZeroList - Given X, creates a 0-List
 * of size X.
 *
 * Parameters :
 * 		(X, ZeroList).
 */		
createZeroList(0, []).
createZeroList(X, [0|T]) :-
	X > 0,
	X1 is X-1,
	createZeroList(X1,T).
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