/*room(3, 3).
booths(3).
dimension(1, 2, 1).
dimension(2, 2, 1).
dimension(3, 1, 1).
position(1, 0, 1).
position(2, 1, 2).
position(3, 0, 0).
target(3, 0, 2).
horizon(10).
*/

/*
room(3,3).
booths(2).
dimension(1,1,1).
dimension(2,1,2).
position(1,0,0).
position(2,1,0).
target(1,0,2).
horizon(10).
*/


room(3,3).
booths(1).
dimension(1,1,1).
%dimension(2,1,2).
position(1,0,0).
%position(2,1,0).
target(1,0,2).
horizon(10).

/*  
 *     0 1 2 3  H
 *   0 ----------> 
 *   1|
 * W 2|
 *   3|
 *   \/
 */

moves(OriginalRoom, TargetRoom, K) :-
	move(OriginalRoom, OriginalRoom, TargetRoom, K, 0).

move(OriginalRoom, Room, NewRoom, 1, Depth) :-
	horizon(Limit),
	Depth =< Limit,
	move1(Room, _, NewRoom),
	target(Booth, StartX, StartY),
	dimension(Booth, Dx, Dy),
	EndX is StartX + Dx - 1,
	EndY is StartY + Dy - 1,
	getAllPos(StartX, StartY, EndX, EndY, TargetPositions),
	checkBoothPos(NewRoom, TargetPositions, Booth, Count),
	length(TargetPositions, TotalCount),
	TotalCount =:= Count,
	% Start checking if the NewRoom matches with OldRoom
	clear(NewRoom, TargetPositions, NewRoom1),
	findpos(OriginalRoom, Booth, InitialPositions),
	clear(OriginalRoom, InitialPositions, Room1),
	equals(NewRoom1, Room1).
	
%TODO Add the condition that the other items should be in the same place.

move(OriginalRoom, Room, NewRoom, K, Depth) :-
	horizon(Limit),
	Depth =< Limit,
	move1(Room, _, NewRoom1),
	Depth1 is Depth + 1,
	move(OriginalRoom, NewRoom1, NewRoom, K1, Depth1),
	K is K1+1.


/* equals - Given 2 matrices, checks if they are equal.
 *
 * Parameters :
 * 		(Matrix1, Matrix2).
 */	
equals([FirstRow | Rest], Matrix2) :-
	equals([FirstRow|Rest], Matrix2, Len),
	length(FirstRow, ColSize),
	length([FirstRow|Rest], RowSize),
	Len =:= ColSize* RowSize.

equals([],[],0).
equals([[]|T],[[]|T1],Len) :-
	equals(T,T1, Len).
equals([[X|T1]|T2], [[X|T3]|T4], Len) :-
	equals([T1|T2], [T3|T4], Len1),
	Len is Len1 + 1.

equals([[X|T1]|T2], [[Y|T3]|T4], Len) :-
	X \= Y,
	equals([T1|T2], [T3|T4], Len).
	

/* checkBoothPos - Given a Room, booth number and list of Positions
 * we check how many positions in the list have booth in it. We
 * return the count of number of such positions.
 *
 * Parameters :
 * 		(Matrix, Positions, BoothNumber, Count).
 */	
checkBoothPos(_, [], _, 0).
checkBoothPos(Room, [[X,Y] | Rest], Booth, Count) :-
	checkBoothPos(Room, Rest, Booth, Count1),
	(cell(Room, X, Y, Booth)
	 -> Count is Count1 + 1
	 ; Count is Count1).
 
/* move1 - Move the Booth by one move by choosing
 * one of the following moves.
 * MoveUp, MoveDown, MoveRight, MoveLeft
 */
 
move1(Room, Booth, NewRoom):-
	moveUp(Room, Booth, NewRoom);
	moveDown(Room, Booth, NewRoom);
	moveLeft(Room, Booth, NewRoom);
	moveRight(Room, Booth, NewRoom).
	
moveUp(Room, Booth, NewRoom) :-
	moveGen(Room, Booth, -1, 0, NewRoom).

moveDown(Room, Booth, NewRoom) :-
	moveGen(Room, Booth, 1, 0, NewRoom).
	
moveRight(Room, Booth, NewRoom) :-
	moveGen(Room, Booth, 0, 1, NewRoom).

moveLeft(Room, Booth, NewRoom) :-
	moveGen(Room, Booth, 0, -1, NewRoom).

/* moveGen - This is a generic move function which given
 * a room and booth moves the booth based on Dx and Dy
 * ONLY if it is legal.
 *
 * Parameters :
 * 		(Matrix, Positions, NewMatrix).
 */	
moveGen(Room, Booth, Dx, Dy, NewRoom):-
	dimension(Booth,_,_),
	findpos(Room, Booth, Positions),
	Positions \= [],
	getNewPos(Positions, [Dx, Dy], NewPositions),
	validPositions(NewPositions, Valid),
	length(Positions, Len),
    Len	=:= Valid,
	legalPositions(Room, Booth, NewPositions, Legal),
	Len =:= Legal,
	clear(Room, Positions, NewRoom1),
	replace(NewRoom1, Booth, NewPositions, NewRoom).

/* clear - Given a Matrix and a list of Positions, clears
 * all the values at these positions to 0.
 *
 * Parameters :
 * 		(Matrix, Positions, NewMatrix).
 */	
clear(Matrix, Positions, NewMatrix) :-
	replace(Matrix, 0, Positions, NewMatrix).
	
/* replace - Given a Matrix, a set of positions and a Value
 * replaces those positions in the matrix with Value and
 * returns a new Matrix.
 *
 * Parameters :
 * 		(Matrix, NewValue, ListOfCoordinates, NewMatrix).
 */	
replace(Matrix, _, [], Matrix).
replace(Matrix, Value, [[X,Y]|Rest], NewMatrix) :-
	replace2(Matrix, X, Y, Value, Matrix1),
	replace(Matrix1, Value, Rest, NewMatrix).
	
	
replace2(Matrix, X, Y, Value, NewMatrix) :-
	replace2(Matrix, X, Y, Value, NewMatrix, 0).

replace2([Row|Rest], X, Y, Value, [NewRow|Rest], X) :-
	replace1(Row, Y, Value, NewRow).

replace2([Row|Rest], X, Y, Value, [NewRow|NewRest], CurrX) :-
	X > CurrX,
	NewRow = Row,
	CurrX1 is CurrX + 1,
	replace2(Rest, X, Y, Value, NewRest, CurrX1).
	
replace1(Row, X, Value, NewRow) :-
	replace1(Row, X, Value, NewRow, 0).
	
replace1([_|T], X, Value, [Value|T], X).
replace1([H|T], X, Value, [H|NewT], CurrX) :-
	X > CurrX,
	CurrX1 is CurrX + 1,
	replace1(T, X, Value, NewT, CurrX1).

	
/* getNewPos - Given a list of co-ordinates and a change vector
 * returns a new list of co-ordinates after applying change vector
 * to each of the original co-ordinates
 *
 * Parameters :
 * 		(ListOfCoordinates, [Dx, Dy], NewListOfCoordinates).
 */	
getNewPos([], _, []).
getNewPos([[X,Y]|Rest], [Dx, Dy], [[NewX,NewY] | NewRest]) :-
	NewX is X + Dx,
	NewY is Y + Dy,
	getNewPos(Rest, [Dx,Dy], NewRest).

getAllPos(StartX, _, EndX, _, []) :-
	StartX > EndX.
	
getAllPos(StartX, StartY, EndX, EndY, Positions) :-
	StartX =< EndX,
	StartX1 is StartX + 1,
	getAllPos(StartX1, StartY, EndX, EndY, Positions1),
	range(StartY, EndY, RangeList),
	addIdToList1(RangeList, StartX, Positions2),
	append(Positions2, Positions1, Positions).
	
	
/* validPositions - Given a list of co-ordinates, returns
 * the count of no. of valid positions. By valid, I'm ONLY
 * checking if the position is inside the room or not.
 *
 * Parameters :
 * 		(ListOfCo-ordinates, Count).
 */
validPositions([], 0).

validPositions([[X,Y]|L], Count) :-
	room(W, H),
	validPositions(L, Count1),
	(X >= 0, X < W,
	 Y >= 0, Y < H
	 -> Count is Count1+1
	 ; Count is Count1).
	
/* legalPositions - Given the room, a booth and a set of new
 * positions, we determine if we can move the booth to these
 * positions. Basically, we're checking if there are obstacles.
 * We return the number of new positions are available for the
 * booth to move.
 *
 * Parameters :
 * 		(Room, BoothNo, NewPositions, LegalCount).
 */
legalPositions(_, _, [], 0).
legalPositions(Room, Booth, [[X,Y]|Rest], Count) :-
	cell(Room, X, Y, Value),
	legalPositions(Room, Booth, Rest, Count1),
	((Value =:= Booth ; Value =:= 0) 
	  -> Count is Count1 + 1
	  ; Count = Count1).
	
/* cell - Given the matrix(list of Lists) and (X,Y),
 * returns the value matrix[X][Y].
 *
 * Parameters :
 * 		(Matrix, XCoord, YCoord, Value).
 */
cell([FirstRow|Rest], X, Y, Value) :-
	cell2([FirstRow|Rest], X, Y, Value).

cell2(ListOfPos, X, Y, Value) :-
	cell2(ListOfPos, X, Y , Value, 0).
	
cell2([FirstRow|_], X, Y, Value, X) :-
	cell1(FirstRow, Y, Value).

cell2([_|Rest], X, Y, Value, CurrX) :-
	X > CurrX,
	CurrX1 is CurrX + 1,
	cell2(Rest, X, Y, Value, CurrX1).

cell1(Row, Pos, Value) :-
	cell1(Row, Pos, Value, 0).

cell1([H|_], Pos, H, Pos).
cell1([_|T], Pos, Value, CurrPos) :-
	Pos > CurrPos,
	CurrPos1 is CurrPos + 1,
	cell1(T, Pos, Value, CurrPos1).
	
	
/* findpos - Given a Room and Booth number, returns
 * the list of positions in which Booth is present
 *
 * Parameters :
 *      (Room, BoothNumber, PositionsList).
 * Eg:
 * 	findpos([[2,0,0], [0,2,0], [1,2,1]], 2, [[2,1],[1,1],[0,0]]);
 */
findpos(Room, Booth, Positions) :-
	findpos(Room, Booth, Positions, [], 0).
	
findpos([], _, Positions, Positions, _).
findpos([FirstRow | Rest], Booth, Positions, Acc, Id) :-
	Id1 is Id+1,
	allindex(FirstRow, Booth, RowPosList),
	addIdToList1(RowPosList, Id, RowPositions),
	(RowPositions \= []
	 ->
	 append(RowPositions, Acc, NewAcc),
	 findpos(Rest, Booth, Positions, NewAcc, Id1);
	 findpos(Rest, Booth, Positions, Acc, Id1)).
	
/* append - Appends lists L1 and L2 to give L
 *
 * Parameters :
 *		(L1,L2,L).
 */
append([], L, L).
append([H|T], L2, [H|L]) :-
	append(T, L2, L).
	
/* length - Length of list
 */
length([], 0).
length([_|T], Len) :- 
	length(T, Len1), 
	Len is (Len1+1).
	
/* AddIdToList1 - takes a list, prepend Id to each member
 * of the list and returns a list of lists with each list
 * as [Id, member].
 *
 * Parameters :
 * 		(List, Id, Result).
 * Eg: addIdToList([1,2,3], 4, [[4,1],[4,2],[4,3]]).
 */
addIdToList1([],_,[]).
addIdToList1([H|T], Id, [[Id,H]|Rest]):-
	addIdToList1(T, Id, Rest).

/* reverse - reverses list L to R
 *
 * Parameters :
 * 		(List,ReverseList).
 */
reverse(L, R) :- reverse(L, [], R).
reverse([], Acc, Acc).

reverse([H|T], Acc, R) :-
	reverse(T, [H|Acc], R).

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
	
/* allindex - Given a List L, Result consists of all the indexes
 * where L[index] = X. 
 *
 * Parameters :
 *		(List, Number, Result)
 */
allindex(L, X, Result):-
	allindex(L, X, Result1, [], 0),
	reverse(Result1, Result).
	
allindex([], _, Result, Result, _).

allindex([H|T], H, Result, Acc, Id) :-
	Id1 is Id+1,
	allindex(T, H, Result, [Id|Acc], Id1).
	
allindex([H|T], X, Result, Acc, Id) :-
	X \= H,
	Id1 is Id+1,
	allindex(T, X, Result, Acc, Id1).