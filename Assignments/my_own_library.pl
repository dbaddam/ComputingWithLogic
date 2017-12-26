
%-----------------------------------------------%-----------------------------------------------%
% member says if that element is present in the given list or not

member(X, [X|T]).

member(X, [H|T]) :-
	member(X, T).

%-----------------------------------------------%-----------------------------------------------%

first([],0).
first([H|T], H).
%-----------------------------------------------%-----------------------------------------------%
% last gives the last element of the array

last([],0).
last([T], T).
last([H|T], X):-
	last(T, X).
%-----------------------------------------------%-----------------------------------------------%
length([],0).

length([H|T],X):-
	length(T, N),
	X is N+1.

%-----------------------------------------------%-----------------------------------------------%
%kth element of a list 

kth([H|_], 1, H).
kth([_|T], K, X):-
	K > 1,
	N is K-1,
	kth(T, N, X).
%-----------------------------------------------%-----------------------------------------------%
% append two lists

append([], L, L).

append([H|T], L, [H|X]) :-
	append(T, L, X).
%-----------------------------------------------%-----------------------------------------------%
% delete a given element at its first occurence

delete([H|T],H, T).

delete([H|T], X, [H|L]) :-
	delete(T, X, L).


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
% input = [a,a,a,b,b,c,c,c]
% output = [[a,a,a],[b,b],[c,c,c]]

pack([H|T],P):- pack1(T, [[H]], P).

pack1([H|T], [[H|L]|AccBefore], AccAfter):-
	pack1(T, [[H,H|L]|AccBefore], AccAfter).

pack1([H|T], [[X|L]|AccBefore], AccAfter):-
	X \= H,
	pack1(T, [[H],[X|L]|AccBefore], AccAfter).

pack1([], AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%

flat([], []).

flat([[]|T], L):- flat(T, L).

flat([[H|T]|T1], [H|P]):-
	flat([T|T1], P).

%-----------------------------------------------%-----------------------------------------------%
contains([H|T], H).

contains([H|T], X) :-
	contains(T, X).
%-----------------------------------------------%-----------------------------------------------%

compress([], []).
compress([X], [X]).

compress([X,Y | T], [X|L]) :-
	X \= Y,
	compress([Y|T], L).

compress([X1|[X1 | T1]], [X1 | L1]) :-
	compress([X1 | T1], [X1|L1]).

%-----------------------------------------------%-----------------------------------------------%
unique([],[]).
unique([X],[X]).

unique([X,Y|L1],[X|T]) :- 
		X \= Y, unique([Y|L1],T).

unique([X|[X|L1]], [X|T]) :- 
	unique([X|L1], [X|T]).
%-----------------------------------------------%-----------------------------------------------%
% input = [a,a,a,b,b,c,c,c]
% output = [[3,a],[2,b],[3,c]]

encode(L, X):-
	pack(L, P),
	encode1(P, [], X).

encode1([], AccBefore, AccAfter) :-
	rev(AccBefore, AccAfter).

encode1([H|T], AccBefore, AccAfter) :-
	length(H, N),
	H = [H1|T1],
	encode1(T, [[N,H1]|AccBefore], AccAfter).



%-----------------------------------------------%-----------------------------------------------%
% input = [3,a]
% output = [a,a,a]

expand(H,L) :- expand1(H, [], L).

expand1([N, H1], AccBefore, AccAfter):-
	N>0,
	N1 is N-1,
	expand1([N1, H1], [H1|AccBefore], AccAfter).

expand1([0|H1], AccBefore, AccBefore).

%-----------------------------------------------%-----------------------------------------------%
% input = [[3,a],[2,b],[3,c]]
% output = [a,a,a,b,b,c,c,c]


decode([H|T],P) :- decode1([H|T], [], P).

decode1([H|T], AccBefore, AccAfter):-
	expand(H,L),
	decode1(T, [L|AccBefore], AccAfter).

decode1([], AccBefore, AccAfter):-
	rev(AccBefore, AccAfter).
%-----------------------------------------------%-----------------------------------------------%
% input = range(4,9,L)
% output = [4,5,6,7,8,9]

range(A,B, L) :- range(A,B,[],L).


range(A, B, AccBefore, AccAfter):- 
	M is A+1,
	N = M,
	N =< B,
	range(N, B, [A| AccBefore], AccAfter).

range(B, C, AccBefore, AccAfter) :-
	B == C,
	rev([B|AccBefore], AccAfter).
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
% remove an element from a list from a given position

remove(L, P, X) :- remove(L, P, [], X).

remove([H|T], P, AccBefore, AccAfter):-
	length([H|T], N),
	N1 is N+1,
	P < N1,
	P1 is P-1,

	(P1 \= 0
		-> remove(T, P1, [H|AccBefore], AccAfter)
		;	remove(T, P1, AccBefore, AccAfter)
	).

remove([],P,AccBefore,AccAfter):-
	rev(AccBefore, AccAfter).

%-----------------------------------------------%-----------------------------------------------%
%-----------------------------------------------%-----------------------------------------------%
contains([H|T], H).

contains([H|T], X) :-
	contains(T, X).
%-----------------------------------------------%-----------------------------------------------%
% append two lists

append([], L, L).

append([H|T], L, [H|X]) :-
	append(T, L, X).

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

kth([H|_], 1, H).
kth([_|T], K, X):-
	K > 1,
	N is K-1,
	kth(T, N, X).
%-----------------------------------------------%-----------------------------------------------%	
% reverse a list using accumulater

rev(L,P) :- rev(L, [], P).

rev([H|T], AccBefore, AccAfter) :-
	rev(T, [H|AccBefore], AccAfter).

rev([], AccBefore, AccBefore).

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
% remove an element from a list from a given position

remove(L, P, X) :- remove(L, P, [], X).

remove([H|T], P, AccBefore, AccAfter):-
	length([H|T], N),
	N1 is N+1,
	P < N1,
	P1 is P-1,

	(P1 \= 0
		-> remove(T, P1, [H|AccBefore], AccAfter)
		;	remove(T, P1, AccBefore, AccAfter)
	).

remove([],P,AccBefore,AccAfter):-
	rev(AccBefore, AccAfter).

%-----------------------------------------------%-----------------------------------------------%
% shift all the elements in a list to RIGHT by 1 position, padding 0s to the left
% input [1,2,3]
% output [0,1,2]

shift_right(L, X) :- 
	append([0],L, L1),
	length(L1, N),
	remove(L1, N, X).
%-----------------------------------------------%-----------------------------------------------%
% shift all the elements in a list to LEFT by 1 position, padding 0s to the left
% input [1,2,3]
% output [2,3,0]

shift_left(L, X) :- 
	append(L,[0],L1),
	remove(L1, 1, X).

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