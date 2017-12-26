

satisfaction(S):-
	people(P),

	places(N),
	create_1_to_N_list(N, L),

	findall(Y, get_satisfaction_all(L,P,X,Y),Z),
	%writeln(Z),

	max(Z, Ans),
	Ans = S.


% given the list of possible locations that can be visited, count the satisfaction number of all persons

get_satisfaction_all(L, P, C, MinValue):-
	permute(L, L1),
	get_locations_possible(L1, LP),
	%writeln('The location possible: '),
	%writeln(LP),
	get_satisfaction_all(LP, P, 0, C, [], X),
	%write('The satisfaction values of all persons = '),%writeln(X),
	%write('the total satisfaction all persons = '),
	%writeln(C),

	min(X, MinValue).
	%write('the Min satisfaction value of a person = '),%writeln(MinValue).

get_satisfaction_all(L, P, C, X, AccBefore, AccAfter):-
	
	(P > 0
		->	%write('counting satisfaction of person = '),%writeln(P),
			get_satisfaction_person(L, P, C1),
			%writeln(C1),
			C2 is C+C1,
		%	%writeln(C2),
			P1 is P-1,
			get_satisfaction_all(L, P1, C2, X, [C1| AccBefore], AccAfter)
		;
			X=C,
			%writeln('Total satisfaction of all persons'),
			%writeln(C),
			AccAfter = AccBefore
	).



get_satisfaction_person(L, P, C):-
	findall(X,prefer(P, X),PrefList),
	get_satisfaction_person(L, P, PrefList, 0, C).

get_satisfaction_person(L, P, [H|T], S, C):-
	(contains(L, H)
			->
				S1 is S+1,
				get_satisfaction_person(L, P, T, S1, C)
			;
				get_satisfaction_person(L, P, T, S, C)
	).

get_satisfaction_person(L, P, [], S, C):-
	findall(X,prefer(P, X),PrefList),
	length(PrefList, N),
	places(PlaceCount),
	(N==S
		->	C = PlaceCount
		;	C = S
	).


% given an input list of all preference permutations possible, find if all those locations can be covered or not based on the visit duration time, close time and open time
% assume that the list input is the order in which the locations are visited, so check if all the locations can be visited in the order specified.

get_locations_possible(L,X):-
	L = [H|T],
	place(H, Duration, OpenTime, CloseTime),
	EndTime is OpenTime + Duration,
	get_locations_possible(T, EndTime, Z),
	X = [H|Z].


get_locations_possible(T, EndTime, X):-
	get_locations_possible(T, EndTime, [], X).

get_locations_possible([T1|T2], EndTime, AccBefore, AccAfter):-

	(is_loc_possible(T1, EndTime, EndTimeThisLoc)
			->
				get_locations_possible(T2, EndTimeThisLoc, [T1|AccBefore],AccAfter)

			;
				get_locations_possible(T2, EndTime, AccBefore, AccAfter)
	).

get_locations_possible([], EndTime, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).





is_loc_possible(L, EndTimePrevLoc, EndTimeThisLoc):-
	place(L, D, O, C),
	EndTimePrevLoc >= O,
	LeastStartTime is C-D,
	LeastStartTime >= EndTimePrevLoc,
	EndTimeThisLoc is EndTimePrevLoc + D.


%-----------------------------------------------%-----------------------------------------------%
% reverse a list

rev([],[]).
rev([H|T], X) :-
	rev(T, Z),
	append(Z, [H], X).

% reverse a list using accumulater

rev(L,P) :- rev(L, [], P).

rev([H|T], AccBefore, AccAfter) :-
	rev(T, [H|AccBefore], AccAfter).

rev([], AccBefore, AccBefore).
%-----------------------------------------------%-----------------------------------------------%
% append two lists

append([], L, L).

append([H|T], L, [H|X]) :-
	append(T, L, X).

%-----------------------------------------------%-----------------------------------------------%
contains([H|T], H).

contains([H|T], X) :-
	contains(T, X).

%-----------------------------------------------%-----------------------------------------------%

permute([], []).

permute([X|Xs], Ys) :-
	permute(Xs, Zs),
	delete(X, Ys, Zs).

%-----------------------------------------------%-----------------------------------------------%	

% create a list with given N by incrementing from 1 to N

create_1_to_N_list(N, X):-
	create_1_to_N_list(N, [], X).


create_1_to_N_list(N, L, X):-
	N>0,
	N1 is N-1,
	create_1_to_N_list(N1, [N|L], X).

create_1_to_N_list(0, L, L).


%-----------------------------------------------%-----------------------------------------------%
% delete a given element at its first occurence

delete(X,[],_) :- fail.% not needed

delete(X, [X|Ys], Ys).

delete(X, [Y|Ys], [Y|Zs]) :-
	delete(X, Ys, Zs).



%-----------------------------------------------%-----------------------------------------------%
% find Minimum element of a list.
% min(L,X).

lowest(A, B, X):-
	(A =< B
		-> X = A
		;  X = B).

min([H|T], X) :- min([H|T], H, X).

min([H|T], V, X):-
	lowest(H, V, Z),
	min(T, Z, X).

min([], V, X):- X = V.


%-----------------------------------------------%-----------------------------------------------%	
getMthElementFromNestedList(L, N, X):- getMthElementFromNestedList(L, N, [], X).

getMthElementFromNestedList([H|T], N, AccBefore, AccAfter):-
	kth(H, N, E),
	getMthElementFromNestedList(T, N, [E| AccBefore], AccAfter).

getMthElementFromNestedList([], N, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter). 


%-----------------------------------------------%-----------------------------------------------%

kth([H|_], 1, H).
kth([_|T], K, X):-
	K > 1,
	N is K-1,
	kth(T, N, X).


%-----------------------------------------------%-----------------------------------------------%
% return first occurence position of a given element

first_occurence(L, E, P):-
	contains(L, E),
	!,
	first_occurence(L, 1, E, P).

first_occurence([H|T], N, E, P):-
	(H == E
		-> P = N

		; N1 is N+1,
		  first_occurence(T, N1, E, P)
	).




%-----------------------------------------------%-----------------------------------------------%
% find Maximum element of a list.
% max(L, X).

highest(A, B, X):-
	(A =< B
		-> X = B
		;  X = A).

max([H|T], X) :- max([H|T], H, X).

max([H|T], V, X):-
	highest(H, V, Z),
	max(T, Z, X).

max([], V, X):- X = V.

%-----------------------------------------------%-----------------------------------------------%
% get all positions of given element in a list



get_all_positions(L, E, X):-
	get_all_positions(L, E, 1, [], X).

get_all_positions([H|T], E, P, AccBefore, AccAfter):-
	P1 is P+1,
	(H==E
		->
			
			get_all_positions(T, E, P1, [P|AccBefore], AccAfter)
		;
			get_all_positions(T, E, P1, AccBefore, AccAfter)
	).

get_all_positions([], E, P, AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%
%get all elements from given positions list

get_elements_from_positions(L, PositionsList, X):-
	get_elements_from_positions(L, PositionsList, [], X).

get_elements_from_positions(L, [H|T], AccBefore, AccAfter):-
	kth(L, H, E),
	get_elements_from_positions(L, T, [E|AccBefore], AccAfter).

get_elements_from_positions(L, [], AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%
length([],0).
%length([[]],0).

length([H|T],X):-
	length(T, N),
	X is N+1.
