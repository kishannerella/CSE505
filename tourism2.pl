/*
people(2).
places(3).
preferences(4).
place(1, 1, 9, 11).
place(2, 1, 9, 11).
place(3, 1, 9, 11).
prefer(1, 1).
prefer(2, 1).
prefer(2, 2).
prefer(2, 3).
*/
people(3).
places(6).
preferences(6).

place(1, 1, 9, 15).
place(2, 1, 9, 15).
place(3, 1, 9, 15).
place(4, 1, 9, 15).
place(5, 2, 9, 15).
place(6, 2, 9, 15).
prefer(1, 1).
prefer(1, 2).
prefer(2, 3).
prefer(3, 4).
prefer(3, 5).
prefer(3, 6).
/* :- satisfaction(K)*/

satisfaction(K) :-
	places(P),
	timingChart(T),
	range(1, P, L),
	preferenceCountList(PrefCountList),
	findall(S, enumerate(L, T, S, PrefCountList), FinalList),
	maximum(FinalList, K).
	
enumerate(P, TimingChart, S, PrefCountList) :-
	perm(P, L),
	computeSatisfaction(L, TimingChart, S, PrefCountList).
	
computeSatisfaction(L, T, S, PrefCountList) :-
	valid(L, T),
	computeSatList(L, PSatList),
	computePersonSatScore(PSatList, PrefCountList, ScoreList),
	minimum(ScoreList, S).
	
minimum([X], X).
minimum([H|T], M) :-
	minimum(T, M1),
	(H < M1 -> M = H; M = M1).
	
maximum([X], X).
maximum([H|T], M) :-
	maximum(T, M1),
	(H > M1 -> M = H; M = M1).
		
	
computePersonSatScore([], [], []).
computePersonSatScore([X|PSatList], [X|PrefCountList], [N|ScoreList]):-
	places(N),
	computePersonSatScore(PSatList, PrefCountList, ScoreList).
	
computePersonSatScore([X|PSatList], [Y|PrefCountList], [X|ScoreList]) :-
	X < Y,
	computePersonSatScore(PSatList, PrefCountList, ScoreList).

preferenceCountList(P) :-
	preferenceCountList1(1, P).
	
preferenceCountList1(PersonNo, P) :-
	people(PNo),
	PersonNo > PNo,
	createZeroList(PNo, P), !.

preferenceCountList1(PersonNo, P) :-
	PersonNo1 is PersonNo + 1,
	preferenceCountList1(PersonNo1, P1),
	preferenceCountList2(PersonNo, LCount),
	updateList(P1, PersonNo, LCount,P).
	
preferenceCountList2(PersonNo, Count) :-
	preferenceCountList3(PersonNo, 1, Count).
	
preferenceCountList3(_, Location, 0) :-
	places(L),
	Location > L.
	
preferenceCountList3(PersonNo, Location, Count) :-
	places(L),
	Location =< L,
	Location1 is Location+1,
	preferenceCountList3(PersonNo, Location1, Count1),
	(prefer(PersonNo, Location) -> Count is Count1 +1; Count = Count1).
	
	
/* Returns a people sized list R where R[i] represents
 * the number of places ith person visited in this trip.i
 * This list follows 1-indexing. 
 * */
computeSatList([], R) :-
	people(PNo),
	createZeroList(PNo, R).
	
computeSatList([H|T], R) :-
	computeSatList1(H, S),
	computeSatList(T, R1),
	addList(R1, S, R).
	
computeSatList1(Place, S) :-
	%people(PNo),
	%createZeroList(PNo, Z),
	computeSatList2(Place, 1, S).
	
computeSatList2(_, PersonNo, S) :-
	people(PNo),
	PersonNo > PNo,
	createZeroList(PNo, S).

computeSatList2(Place, PersonNo, S) :-
	people(PNo),
	PersonNo =< PNo,
	PersonNo1 is PersonNo +1,
	computeSatList2(Place, PersonNo1, S1),
	(prefer(PersonNo, Place)-> updateList(S1, PersonNo, 1, S); S = S1).
	
updateList(L, Id, Value, R) :-
	updateList1(L, Id, Value, R, 1).
	
updateList1([_|T], Id, Value, [Value|T], Id).

updateList1([H|T], Id, Value, [H|T1], CurrId) :-
	Id > CurrId,
	CurrId1 is CurrId + 1,
	updateList1(T, Id, Value, T1, CurrId1).

valid(List, T) :-
	valid1(List, T, Schedule),
	validSchedule(Schedule).
	
valid1([], _, S) :-
	createZeroList(24, S).
valid1([H|T], Chart, Schedule) :-
	findPlaceTimingChart(Chart, H, PlaceChart),
	member(PlaceChart,S),
	valid1(T, Chart, S1),
	addList(S, S1, Schedule).

addList([], [], []).
addList([H1|T1], [H2|T2], [H|T]):-
	H is H1 + H2,
	addList(T1, T2, T).
	
validSchedule([]).
validSchedule([X|T]) :-
	X =< 1,
	validSchedule(T).
	
member([H|_], H).
member([_|T], X) :-
	member(T, X).
	
findPlaceTimingChart(Chart, X, PlaceChart) :-
	findPlaceTimingChart(Chart, X, PlaceChart, 1).
	
findPlaceTimingChart([H|_], Id, H, Id).
findPlaceTimingChart([_|T], Id, PlaceChart, Depth) :-
	Id > Depth,
	Depth1 is Depth+1,
	findPlaceTimingChart(T, Id, PlaceChart, Depth1).
	
timingChart(T) :-
	timingChart1(1, T).

timingChart1(No, []) :-
	places(P),
	P < No.
	
timingChart1(No, [H|T]) :-
	place(No, VTime, Start, End),
	timingChart2(VTime, Start, End, H),
	No1 is No+1,
	timingChart1(No1, T).

timingChart2(VTime, Start, End, []) :-
	VTime + Start > End.
	
timingChart2(VTime, Start, End, [H|T]) :-
	VTime + Start =< End,
	createZeroList(24, Z),
	Upto is Start + VTime,
	replace(Z, Start, Upto, H),
	Start1 is Start+1,
	timingChart2(VTime, Start1, End, T).
	
replace(X, Start, Upto, Y) :-
	replace1(X, Start, Upto, Y, 0).
	
replace1([], _, _, [],_).
replace1([_|T], Start, Upto, [1|T1], Id) :-
	Id >= Start,
	Id < Upto,
	Id1 is Id + 1,
	replace1(T, Start, Upto, T1, Id1).

replace1([_|T], Start, Upto, [0|T1], Id) :-
	(Id < Start;
	Id >= Upto),
	Id1 is Id + 1,
	replace1(T, Start, Upto, T1, Id1).
	
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
	(perm(T, L));
	(perm(T, Z),
	remove(H, L, Z)).
	
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