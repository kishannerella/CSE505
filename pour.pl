/*vessels(4).
source(1).
people(3).
capacity(1, 12).
capacity(2, 5).
capacity(3, 3).
capacity(4, 1).
horizon(10).
*/

vessels(4).
source(1).
people(4).
capacity(1, 12).
capacity(2, 3).
capacity(3, 3).
capacity(4, 3).
horizon(10).

split() :-
	constructList(Tray),
	tryMovesWithDepth(Tray, 0).

tryMovesWithDepth(Tray, D) :-
	writeln(D),
	horizon(Limit),
	D < Limit,
	D1 is D+1,
	(divide(Tray, _, 0, D)
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

divide(Tray, NewTray, Depth, Limit) :-
	Depth < Limit,
	capacity(X,_),
	capacity(Y,_),
	action(Tray, X, Y, NewTray),
	source(S),
	people(P),
	capacity(S, TotalBeer),
	BeerPerPerson is TotalBeer//P,
	%writeln(BeerPerPerson),
	checkTray(NewTray, BeerPerPerson).


divide(Tray, NewTray, Depth, Limit) :-
	Depth < Limit,
	capacity(X,_),
	capacity(Y,_),
	action(Tray, X, Y, Tray1),
	%writeln(Tray1),
	Depth1 is Depth + 1,
	divide(Tray1, NewTray, Depth1, Limit).

/* checkTray - Checks whether the given tray configuration
 * is valid solution.
 *
 * Parameters :
 * 		checkTray(Tray, BeerPerPerson).
 */
checkTray([],_).
checkTray([[_, X] | T], Value) :-
	(X =:= 0 ; X =:= Value),
	checkTray(T,Value).
	
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