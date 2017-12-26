%-----------------------------------------------%-----------------------------------------------%---------------------------------------------------------%
%	room(3, 3). booths(1). dimension(1, 1, 1). position(1, 0, 0). target(1, 2, 2). horizon(10).
%	room(3, 3). booths(3). dimension(1, 2, 1). dimension(2, 2, 1). dimension(3, 1, 1). position(1, 0, 1). position(2, 1, 2). position(3, 0, 0). target(3, 0, 2). horizon(10).


moves(Z) :-
	room(R, C),
	booths(N),
	prepare_empty_list(R, C, EmptyList),
	prepare_booths_list(EmptyList, N, L),
	target(B, _, _),
	prepare_target_list(EmptyList, N, B, Target),
	horizon(HMax),
	iterate_horizon(L, Target, 1, 0, 1, HMax, Z).



iterate_horizon(L, Target, 1, 0, H, HMax, Z):-
	H =< HMax,
	(moves_wrap(L, Target, 1, 0, H, Z)
		->	writeln('')
		;	H1 is H+1,
			iterate_horizon(L, Target, 1, 0, H1, HMax, Z)	
	).



moves_wrap(L, R, X, 0, D, Z):-
	X =< D,
	(moves(L, R, X, 0, D)
		->	Z=X

		;	X1 is X+1,
			moves_wrap(L, R, X1, 0, D, Z)
	).
	



moves(L, L, 0, D, MaxD) :-
	D =< MaxD.

moves(L, R, X, D, MaxD):-
	D =< MaxD,
	position(B, _,_),
	move_block(L, B, R1),
	D1 is D+1,
	moves(R1, R, X1, D1, MaxD),
	X is X1 + 1.

move_block(L, B, P):-
	move_up(L, B, P).

move_block(L, B, P):-
	move_down(L, B, P).

move_block(L, B, P):-
	move_right(L, B, P).

move_block(L, B, P):-
	move_left(L, B, P).


prepare_booths_list(L, N, X):-
	(N < 1
		-> X = L
		;
			dimension(N, DX, DY),
			position(N, PX, PY),
			put(L, N, DX, DY, PX, PY, Z),
			N1 is N-1,
			prepare_booths_list(Z, N1, X)
	).

prepare_target_list(L, N, B, X):-
	(N < 1
		-> X = L
		;
			dimension(N, DX, DY),
			(N == B
				->	target(N, PX, PY)
				;	position(N, PX, PY)
			),
			put(L, N, DX, DY, PX, PY, Z),
			N1 is N-1,
			prepare_target_list(Z, N1, B, X)
	).




prepare_empty_list(R, C, L):-
	fill(R, 0, [], Z),
	fill(C, Z, [], L).

% return a list of given element E with occurences of given size R 

fill(R, E, AccBefore, AccAfter):-
	R >= 0,
	R1 is R-1,
	fill(R1, E, [E|AccBefore], AccAfter).

fill(0, E, AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%
length([],0).
%length([[]],0).

length([H|T],X):-
	length(T, N),
	X is N+1.
%-----------------------------------------------%-----------------------------------------------%
% find Minimum element of a list.

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
% append two lists

append([], L, L).

append([H|T], L, [H|X]) :-
	append(T, L, X).
%-----------------------------------------------%-----------------------------------------------%


contains([H|T], H).

contains([H|T], X) :-
	contains(T, X).

kth([H|_], 1, H).
kth([_|T], K, X):-
	K > 1,
	N is K-1,
	kth(T, N, X).


% reverse a list using accumulater

rev(L,P) :- rev(L, [], P).

rev([H|T], AccBefore, AccAfter) :-
	rev(T, [H|AccBefore], AccAfter).

rev([], AccBefore, AccBefore).
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

% return last occurence position of a given element

last_occurence(L, E, P):-
	contains(L, E),
	!,
	rev(L, L1),
	length(L, N),
	first_occurence(L1, E, P1),
	P is N-P1+1.

%-----------------------------------------------%-----------------------------------------------%

% check if a block is right movable or not, should check if there is 0 to right or not

right_movable(L, B):-
	contains(L, B),
	last_occurence(L, B, P),
	P1 is P+1,
	kth(L, P+1, X),
	X==0.

%-----------------------------------------------%-----------------------------------------------%

% check if a block is left movable or not, should check if there is 0 to left or not

left_movable(L, B):-
	contains(L, B),
	first_occurence(L, B, P),
	P1 is P-1,
	kth(L, P1, X),
	X==0.

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
% delete an element at a given position in given list

delete(L, P, X):-
	length(L, N),
	P > 0,
	N >= P,
	delete(L, P, [], X).

delete([H|T], P, AccBefore, AccAfter):-
	P1 is P-1,
	(P1 == 0
		-> delete(T, P1, AccBefore, AccAfter)
		; delete(T, P1, [H| AccBefore], AccAfter)
	).

delete([], P, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).
%-----------------------------------------------%-----------------------------------------------%
% shift the given element occurences to right in a given list
% NOTE: this is not same as shift_right where all the elements are shifted right in a given list

shift_right_element(L, E, X):-
	contains(L, E),
	right_movable(L, E),

	first_occurence(L, E, M),
	insert(L, 0, M, I),

	last_occurence(I, E, N),
	N1 is N+1,
	delete(I, N1, X).
%-----------------------------------------------%-----------------------------------------------%
% shift the given element occurences to left in a given list
% NOTE: this is not same as shift_left where all the elements are shifted left in a given list

shift_left_element(L, E, X):-
	contains(L, E),
	left_movable(L, E),

	last_occurence(L, E, M),
	M1 is M+1,
	insert(L, 0, M1, I),

	first_occurence(I, E, N),
	N1 is N-1,
	delete(I, N1, X).

%-----------------------------------------------%-----------------------------------------------%
% move a block B to right by 1 step:
% move_right([[3,0,0],[6,1,0],[2,1,0]],1,X).

move_right(L, B, P) :- move_right(L, B, [], P).

move_right([H|T], B, AccBefore, AccAfter) :-
	(contains(H, B) 
		-> shiftBlockRight(H, B, X),
		   move_right(T, B, [X| AccBefore], AccAfter)
		;	move_right(T, B, [H| AccBefore], AccAfter)
	).

move_right([], B, AccBefore, AccAfter) :-
	rev(AccBefore, AccAfter).


shiftBlockRight(H, B, X):- 
	shift_right_element(H, B, X).

%-----------------------------------------------%-----------------------------------------------%
% move a block B to left by 1 step:
% 	

move_left(L, B, P) :- move_left(L, B, [], P).

move_left([H|T], B, AccBefore, AccAfter) :-
	(contains(H, B) 
		-> shiftBlockLeft(H, B, X),
		   move_left(T, B, [X| AccBefore], AccAfter)
		;	move_left(T, B, [H| AccBefore], AccAfter)
	).

move_left([], B, AccBefore, AccAfter) :-
	rev(AccBefore, AccAfter).


shiftBlockLeft(H, B, X):- 
	shift_left_element(H, B, X).

%-----------------------------------------------%-----------------------------------------------%

% Transform  [[a,b],[c,d],[e,f]] into [[a,c,d],[b,d,f]]


transform(L, X):- 
	length(L, M),
	L=[H|T],
	length(H, N),
	transform(L, M, N, [], X).

transform(L, N, M, AccBefore, AccAfter):-
	(M==0
		-> AccAfter = AccBefore

		;	getMthElementFromNestedList(L, M, X),
			M1 is M-1,
			transform(L, N, M1, [X|AccBefore], AccAfter)
	).


getMthElementFromNestedList(L, N, X):- getMthElementFromNestedList(L, N, [], X).

getMthElementFromNestedList([H|T], N, AccBefore, AccAfter):-
	kth(H, N, E),
	getMthElementFromNestedList(T, N, [E| AccBefore], AccAfter).

getMthElementFromNestedList([], N, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter). 
	
%-----------------------------------------------%-----------------------------------------------%
% move a block B down by 1 position
% move_down([[3,1,1],[0,0,0],[0,2,2]], 1, X).

move_down(L, B, P) :- 
	transform(L, LT),
	move_down(LT, B, [], P).

move_down([H|T], B, AccBefore, AccAfter) :-
	(contains(H, B) 
		-> shiftBlockRight(H, B, X),
		   move_down(T, B, [X| AccBefore], AccAfter)
		;	move_down(T, B, [H| AccBefore], AccAfter)
	).

move_down([], B, AccBefore, AccAfter) :-
	rev(AccBefore, L),
	transform(L, AccAfter).

% move a block B up by 1 position
% move_up([[3,0,0],[0,1,1],[0,2,2]], 1, X).

move_up(L, B, P) :- 
	transform(L, LT),
	move_up(LT, B, [], P).

move_up([H|T], B, AccBefore, AccAfter) :-
	(contains(H, B) 
		-> shiftBlockLeft(H, B, X),
		   move_up(T, B, [X| AccBefore], AccAfter)
		;	move_up(T, B, [H| AccBefore], AccAfter)
	).

move_up([], B, AccBefore, AccAfter) :-
	rev(AccBefore, L),
	transform(L, AccAfter).

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

% place an element at given position in a nested list
% place([[0,0,0],[0,0,0],[0,0,0]], 1, 3, 1, X).

place(L, E, X, Y, Z):-
	place(L, E, X, Y, [], Z).

place([H|T], E, X, Y, AccBefore, AccAfter):-
	X1 is X-1,
	(X1 == 0
		-> replace(H, E, Y, Z),
		   place(T, E, X1, Y, [Z|AccBefore], AccAfter)

		;  place(T, E, X1, Y, [H|AccBefore], AccAfter)
	).

place([], E, X, Y, AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).

%-----------------------------------------------%-----------------------------------------------%
% put an element of given dimension DX, DY, at given position PX, PY recursively in a given list
% put([[0,0,0],[0,0,0],[0,0,0]], 1, 1, 1, 0, 1, X).

% swapping and incrementing the position variables here

put(L, E, DX, DY, PX, PY, X):-
	P1 is PX+1,
	P2 is PY+1,
	put_dimension_x(L, E, DX, DY, P2, P1, X).

% put_dimension_x([[0,0,0],[0,0,0],[0,0,0]], 3, 2, 2, 2,2, X).

put_dimension_x(L, E, DX, DY, PX, PY, X):-
	D is DX - 1,
	(D < 0
		-> 	X = L
		;
			
			%place(L, E, PX, PY, Z),
			put_dimension_y(L, E, D, DY, PX, PY, Z),
			PY1 is PY + 1,
			put_dimension_x(Z, E, D, DY, PX, PY1, X)
	).

% given a dimension DY, position PX, PY, in a given list
% put_dimension_y([[0,0,0],[0,0,0],[0,0,0]], 3, 2, 2, 2, X).

put_dimension_y(L, E, DX, DY, PX, PY, X):-
	D is DY - 1,
	(D < 0
		-> 	X = L
		;
			
			place(L, E, PX, PY, Z),
			PX1 is PX + 1,
			put_dimension_y(Z, E, DX, D, PX1, PY, X)
	).
%-----------------------------------------------%-----------------------------------------------%