/*
dishes(3).
separation(1).
hot(1).

table_width(4).
dish_width(1, 1).
dish_width(2, 1).
dish_width(3, 2).
demand(1, 1).
demand(2, 1).
demand(3, 1).
*/

/*
dishes(4).
separation(2).
hot(2).

table_width(4).
dish_width(1, 1).
dish_width(2, 2).
dish_width(3, 2).
dish_width(4, 1).
demand(1, 3).
demand(2, 1).
demand(3, 1).
demand(4, 3).
*/

tables(K) :-
	constructMenu(Menu),
	%findall(X, distribute(Menu, 0, X, 0), L),
	setof(X, distribute(Menu, 0, X, 0), [K|_]).
	%minimum(L, K).
	
	
/*
tryWithTables(Menu, D, K) :-
	K1 is K + 1,
	(distribute(Menu, 0, K1, 0)
	 -> K = K1
	 ;tryWithTables(Menu, K+1)
	)
	*/
distribute(Menu, 0, 0, _) :-
	empty(Menu), !.

distribute(Menu, _, 1, _) :-
	empty(Menu), !.

	
distribute(Menu, LastTableWidth, K, Last) :-
	dish_width(X,_),
	available(Menu, X, Value),
	addDish(X, LastTableWidth, Last, NewTableWidth, AddTable),
	Value1 is Value - 1,
	update(Menu, X, Value1, NewMenu),
	distribute(NewMenu, NewTableWidth, K1, X),
	K is K1 + AddTable.

addDish(X, LastTableWidth, Last, NewTableWidth, AddTable) :-
	((hotDish(Last),hotDish(X)) ; (coldDish(Last), coldDish(X))),
	dish_width(X, DishWidth),
	table_width(TableWidth),
	RemWidth is TableWidth - LastTableWidth,
	(RemWidth > DishWidth
	 -> NewTableWidth is LastTableWidth + DishWidth, AddTable = 0
	 ; (RemWidth =:= DishWidth 
	    -> NewTableWidth = 0, AddTable = 1
		;  (DishWidth =:= TableWidth
		    -> NewTableWidth = 0, AddTable = 2
			; NewTableWidth = DishWidth, AddTable = 1)
		)
	 ).
	

addDish(X, LastTableWidth, Last, NewTableWidth, AddTable) :-
	((hotDish(Last), coldDish(X)) ; (coldDish(Last), hotDish(X))),
	dish_width(X, DishWidth),
	table_width(TableWidth),
	separation(S1),
	(LastTableWidth =:= 0 -> S = 0; S = S1),
	RemWidth is TableWidth - LastTableWidth - S,
	(RemWidth > DishWidth
	 -> NewTableWidth is LastTableWidth + DishWidth + S, AddTable = 0
	 ; (RemWidth =:= DishWidth 
	    -> NewTableWidth = 0, AddTable = 1
		;  (DishWidth =:= TableWidth
		    -> NewTableWidth = 0, AddTable = 2
			; NewTableWidth = DishWidth, AddTable = 1)
		)
	 ).
	
hotDish(X) :-
	hot(H),
	X =< H.
	
coldDish(X) :-
	hot(H),
	X > H.
	
update([[Id, _]| T], Id, Value, [[Id, Value]|T]).

update([[Y, X] | T], Id, Value, [[Y,X] | T1]) :-
	Y \= Id,
	update(T, Id, Value, T1).

available([[Id, Value]|_], Id, Value) :- 
	(Value > 0).
available([[X, _]|T ], Id, Value) :-
	X \= Id,
	available(T, Id, Value).
	
	
	
empty([]).
empty([[_, 0]|T]) :-
	empty(T).
	
/* constructMenu - Gives the demand list in the form of
 * [DishNo, Demand]
 * Parameters :
 * 		constructMenu(List).
 */	
constructMenu(L) :-
	constructMenu(1, L).
	
constructMenu(Id, []) :-
	dishes(D),
	Id > D.
	
constructMenu(Id, [[Id, Val]|T]) :-
	Id > 0,
	demand(Id,Val),
	Id1 is Id+1,
	constructMenu(Id1, T).