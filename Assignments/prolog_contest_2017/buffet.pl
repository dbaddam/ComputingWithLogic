% assume we have got a dish and we should put on a table[0,0,0,0]
% -> check if the (max number of consecutive zeros in table >= length of the dish)
% 		->	check if we are placing the dish beside a different type of dish
%				->	check if (max number of consecutive zeros in table + seperation_distance >= length of the dish)
%				;	put the dish onto the table at the (max_zero_position + seperation_distance).
%
%		;	put the dish onto the table at the max_zero_position.

% input list = [[1,1],[1,1],[1,1],[2,1],[2,0],[1,0],[1,0],[1,0]]

% input table = [0,0,0,0]


tables(K):-
	dishes(N),
	separation(D),
	hot(Hot),

	init(N, Hot, I),
	flat(I, Input),
	%writeln(Input),

	table_width(TWidth),
	create_table(T),
	%writeln(T),

	place_all_dishes([], T, Input, D, 1, Count),
	Count = K, !.



% put all the dishes onto a given table and count number of tables after all the dishes are placed.
% given input list and a table, pick a dish, try to place the dish on that table, 
% 		if not successful, create a new table, increment table counter, place the same dish. 
%		if successful, delete the dish from input list, pick another dish, and place that dish.
% Run this, until your input list becomes empty and then return the table counter value.

% DONE: when placing a dish, we are checking if we can place it on given table and proceeding to create a new one if we cant place it.
%			BUT, we should also check if we can place it on any PREVIOUS tables or not,
%			for which should keep track of all old tables also.

place_all_dishes(Tables_list, T, L, D, C, Count):-
	
	(is_empty(L)
			->	
				%write('Last Table = '),%writeln(T),
				%writeln('add the last table onto table list'),
				add_to_tables_list(Tables_list, T, Added_table_list2),
				%write('Final TABLE LIST = '),%writeln(Added_table_list2),
				Count = C

			;

				(is_table_free(T)
						->
							member(X, L),
							%write('dish picked = '),%writeln(X),
							%write('Trying to place dish on table = '),%writeln(T),
							(place_dish(T, X, D, T1)
									->
										%write('placed dish successfully onto Table = '),%writeln(T1),
										delete(L, X, L1),
										member(L1, Y),
										place_all_dishes(Tables_list, T1, L1, D, C, Count)

									;
										%write('unable to place the picked dish onto given table = '),%writeln(T),

										%writeln('check if dish can be placed onto old table list'),
										(place_dish_on_table_list(Tables_list, X, D, Updated_tables_list)
												->	
													%write('placed dish onto OLD table, Updated_tables_list = '),%writeln(Updated_tables_list),
													delete(L, X, L1),
													member(L1, Y),
													place_all_dishes(Updated_tables_list, T, L1, D, C, Count)
												;

													%writeln('unable to place the dish onto old table list also'),
													%writeln('push table onto tables list'),

													add_to_tables_list(Tables_list, T, Added_table_list),
													%write('updated Tables list after adding = '),%writeln(Added_table_list),


													create_table(T3),
													%write('New table created 1= '),%writeln(T3),
													C2 is C+1,
													place_all_dishes(Added_table_list, T3, L, D, C2, Count)
										)
							)			

						;
							%write('Table full or unable to place a dish onto table = '),%writeln(T),
							add_to_tables_list(Tables_list, T, Added_table_list1),
							%write('table list after adding the above table = '),%writeln(Added_table_list1),

							%writeln('test'),
							member(X, L),
							%write('dish picked = '),%writeln(X),

							(place_dish_on_table_list(Added_table_list1, X, D, Updated_tables_list1)
									->	
										%write('placed dish onto OLD table, Updated_tables_list = '),%writeln(Updated_tables_list1),
										delete(L, X, L2),
										member(L2, Y),
										place_all_dishes(Updated_tables_list1, T, L2, D, C, Count)
									; 
										create_table(T2),

										%write('New table created 2= '),%writeln(T2),
										C1 is C+1,
										place_all_dishes(Added_table_list1, T2, L, D, C1, Count)			
							)
							
				)
	).


% given a table list, check if we can place it on any previous table,
% if so, place it on that table amd update the table list,
% if not return false


place_dish_on_table_list(Tables_list, E, D, X):-
	%writeln('---- in place_dish_on_table_list method ----'),
	Tables_list = [Table|Rest],
	%write('trying to place dish on table = '),%writeln(Table),
	(place_dish(Table, E, D, Updated_table)
			->
				%writeln(Updated_table),
				replace_table(Tables_list, Table, Updated_table, X)
			;
				place_dish_on_table_list(Rest, E, D, X)
	).


replace_table(Tables_list, Table, Updated_table, X):-
	replace_element(Tables_list, Table, Updated_table, X).



add_to_tables_list(Tables_list, T, Added_table_list):-
	add(Tables_list, T, Added_table_list).

%	NOTE: As of now, iam placing the dish at the last occurence of the free space on the table.
%	Above note can be considered invalid, as code has been written to handle that case too.



place_dish(L, E, D, X):-
	is_table_free(L),
	%writeln(E),
	E = [Length|Type],
	max_consecutive_zeros(L, Empty_space, Z),
	Empty_space >= Length,
	(place_dish(L, E, D, Empty_space, Z, X)
			->	true
			;	
				To is Z+Empty_space,
				replace_from_to(L, Z, To,-1, L1),
				max_consecutive_zeros(L1, E1, Z1),
				place_dish(L1, E, D, J),
				replace_all(J, -1, 0, X)
	).

place_dish(L, E, D, Empty_space, Z, X):-
	is_table_free(L), 
	E = [Length|Type],

	Empty_space >= Length,
	Z1 is Z-1,
	(is_same_type(L, E, Z1)
		->	Z3 is Z+D+Length,
			Z5 is Z+Length,
			is_same_type(L, E, Z5),
			is_same_type(L, E, Z3),
			place_dish_at(L, E, Z, X)

		;	
			Needed_empty_space is Length+D,
			Needed_empty_space =< Empty_space,
			Z2 is Z+D,
			is_table_free_at(L, Z2),
			Z4 is Z+D+Length+1,
			is_same_type(L, E, Z4),
			place_dish_at(L, E, Z2, X)
	).


% shall replace 0s multiple times depending on length of the dish.
place_dish_at(L, E, P, X):-
	E = [H|T],
	place_dish_at(L, E, H, P, X).

place_dish_at(L, E, Size, P, X):-
	(Size > 0
		->	Size1 is Size - 1,
			replace(L, E, P, Z),
			P1 is P+1,
			place_dish_at(Z, E, Size1, P1, X)

		;	X=L
	).




is_table_free(L):-
	contains(L, 0).

is_table_free_at(L, P):-
	contains(L, 0, P).


%-----------------------------------------------%-----------------------------------------------%
init(N, H, X):-
	Cold is N-H,
	create_hot_dishes(H, Z, 1),
	create_cold_dishes(H, Y, N),
	append(Z,Y,X).

create_hot_dishes(H, Z, C):-
	create_hot_dishes(H, [], Z, C).

create_hot_dishes(H, AccBefore, AccAfter, C):-
	(C =< H
		->
			C1 is C+1,
			dish_width(C, W),
			demand(C, T),
			create_dish(C, W, T, 1, Z),
			create_hot_dishes(H, [Z|AccBefore], AccAfter, C1)
		;
			AccAfter = AccBefore
	).

create_cold_dishes(H, Y, C):-
	create_cold_dishes(H, [], Y, C).

create_cold_dishes(H, AccBefore, AccAfter, C):-
	(C > H
		->
			C1 is C-1,
			dish_width(C, W),
			demand(C, T),
			create_dish(C, W, T, 0, Y),
			create_cold_dishes(H, [Y|AccBefore], AccAfter, C1)
		;
			AccAfter = AccBefore
	).
			

create_dish(C, W, Demand, Type, X):-
	create_dish(C, W, Demand, Type, [], X).

create_dish(C, W, Demand, Type, AccBefore, AccAfter):-
	Demand1 is Demand-1,
	(Demand1 >= 0
		->	create_dish(C, W, Demand1, Type, [[W,Type]|AccBefore], AccAfter)
		;	AccAfter = AccBefore
	).



create_table(L):-
	table_width(TWidth),
	fill(TWidth, 0, [], L).

fill(R, E, AccBefore, AccAfter):-
	R >= 0,
	R1 is R-1,
	fill(R1, E, [E|AccBefore], AccAfter).

fill(0, E, AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%

%-----------------------------------------------%-----------------------------------------------%
% insert a given element at given position in given list

insert(L, E, P, X):-
	length(L, N),
	P >0,
%	N >= P,
	P1 is P-1,
	(N==P1
		-> append(L, [E], X)
		;insert(L, E, P, [], X)
	).

insert([H|T], E, P, AccBefore, AccAfter):-
	P1 is P-1,
	(P1 == 0
		-> insert(T, E, P1, [H, E|AccBefore], AccAfter)
		;  insert(T, E, P1, [H | AccBefore], AccAfter)
	).

insert([], E, P, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).
%-----------------------------------------------%-----------------------------------------------%	
getMthElementFromNestedList(L, N, X):- getMthElementFromNestedList(L, N, [], X).

getMthElementFromNestedList([H|T], N, AccBefore, AccAfter):-
	kth(H, N, E),
	getMthElementFromNestedList(T, N, [E| AccBefore], AccAfter).

getMthElementFromNestedList([], N, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter). 
%-----------------------------------------------%-----------------------------------------------%	
% return the position of a given element in a given list
% NOTE: Ignoring the case if there are multiple elements present in the given list
% returns the position of the first occurence of a given element.

position(L, E, X):-
	position(L, E, 0, X).

position([H|T], E, P, X):-
	P1 is P+1,
	(H == E
		->	X = P1
		;	position(T, E, P1, X)
	).
%-----------------------------------------------%-----------------------------------------------%	
kth([H|_], 1, H).
kth([_|T], K, X):-
	K > 1,
	N is K-1,
	kth(T, N, X).


%-----------------------------------------------%-----------------------------------------------%

flat([], []).

flat([[]|T], L):- flat(T, L).

flat([[H|T]|T1], [H|P]):-
	flat([T|T1], P).
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
contains(L, E, P):-
	kth(L, P, K),
	K == E.
%-----------------------------------------------%-----------------------------------------------%	
rev(L,P) :- rev(L, [], P).

rev([H|T], AccBefore, AccAfter) :-
	rev(T, [H|AccBefore], AccAfter).

rev([], AccBefore, AccBefore).
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
length([],0).
%length([[]],0).

length([H|T],X):-
	length(T, N),
	X is N+1.
%-----------------------------------------------%-----------------------------------------------%
% check if the list contains of all the same given element type

all_same([], E).

all_same([H|T], E):-
	H == E,
	all_same(T, E).
%-----------------------------------------------%-----------------------------------------------%	
% find maximum consecutives zeros in a given list, return 0 if there are no zeros in the list, ignore position(rubbish) if no 0s.
% NOTE: Ignoring the case if there are multiple 0s present in the given list
% returns the position of the last occurence of maximum consecutives zeros.

% X = maximum consecutives zeros
% Y = position of first occurence 

max_consecutive_zeros(L, X, Y):-
	length(L, N),
	max_consecutive_zeros(L, 0, 0, [], N, X, Y).
	

max_consecutive_zeros([H|T], P, C, Buffer, N, X, Y):-
	P1 is P+1,
	(H == 0
		-> 	C1 is C+1,
			max_consecutive_zeros(T, P1, C1, Buffer, N, X, Y)

		;	max_consecutive_zeros(T, P1, 0, [[P1,C]| Buffer], N, X, Y)
	).			
	
max_consecutive_zeros([], P, C, Buffer, N, X, Y):-
	(P == N
		->	P1 is P+1
	),
	getMthElementFromNestedList([[P1,C]| Buffer], 2, Buffer_count),
	max(Buffer_count, Max),
	position(Buffer_count, Max, Index),
	getMthElementFromNestedList([[P1,C]| Buffer], 1, Buffer_position),
	kth(Buffer_position, Index, Z),
	Y is Z-Max,
	X = Max.



max_consecutive_zeros_but_not(L, X, Y, P, R):-
	replace(L, -1, P, Z),
	R=L,
	max_consecutive_zeros(Z, X, Y).

%-----------------------------------------------%-----------------------------------------------%
% given an [element] and a position and nested list, check if  element1[position] similar to element ??
% if we receive position as 0, no need to check the adjacent dish, just return true.

n:-
	is_same_type([[1,1],[1,1],0,0], [2,1], -1).


is_same_type(L, E, 0).

is_same_type(L, E, P):-
	length(L, N),
	P > N.

is_same_type(L, E, P):-
	kth(L, P, K),
	(K \= 0
		->
			K=[H|T],
			E=[H1|T1],
			T == T1

		;	true
	).

%-----------------------------------------------%-----------------------------------------------%
% replace an element in a list at given position
% L input list
% E element 
% P position
% X output list

% replace([1,2,0,0,0],7,4,X).

replace(L, E, P, X) :- replace(L, E, P, [], X).

replace([H|T], E, P, AccBefore, AccAfter) :-
	length([H|T], N),
	N1 is N+1,
	P < N1,
	P1 is P-1,

	(P1 \= 0 
		-> replace(T, E, P1, [H| AccBefore], AccAfter)
		;   replace(T, E, P1, [E| AccBefore], AccAfter)
	).

replace([], E, P, AccBefore, AccAfter) :-
	rev(AccBefore, AccAfter).


%-----------------------------------------------%-----------------------------------------------%
% replace_all elements of given type of A with B

replace_all(L, A, B, X):-
	replace_all(L, A, B, [], X).

replace_all([H|T], A, B, AccBefore, AccAfter):-
	(H == A
		->	replace_all(T, A, B, [B|AccBefore], AccAfter)
		;	replace_all(T, A, B, [H|AccBefore], AccAfter)
	).

replace_all([], A, B, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).

%-----------------------------------------------%-----------------------------------------------%
% replace_all elements from position A to B, with the given element.

replace_from_to(L, A, B, E, X):-
	(A =< B
		->
			replace(L, E, A, X1),
			A1 is A+1,
			replace_from_to(X1, A1, B, E, X)
		;
			X=L
	).

%-----------------------------------------------%-----------------------------------------------%

% delete a given element at its first occurence

delete([H|T],H, T).

delete([H|T], X, [H|L]) :-
	delete(T, X, L).
%-----------------------------------------------%-----------------------------------------------%
is_empty(L):-
	length(L, N),
	N==0.


%-----------------------------------------------%-----------------------------------------------%
member(X, [X|T]).

member(X, [H|T]) :-
	member(X, T).
%-----------------------------------------------%-----------------------------------------------%	

% replace an element E with R in a given  list

replace_element(L, E, R, X):-
	first_occurence(L, E, P),
	replace(L, R, P, X).

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
% add an element E to given list at end.

add(L, E, X):-
	(is_empty(L)
			->	X = [E]
			;	insert(L, E, 1, X)
	).