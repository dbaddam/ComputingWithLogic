

split(X):-
	vessels(V),
	source(S),
	capacity(S, C),
	people(P),
	horizon(H),

	N is C/P,

	fill(V, 0, EmptyList),
	replace(EmptyList, C, S, VesselsList),
	%write('Initial Vessel List: '),writeln(VesselsList),

	
	(shuffle(VesselsList, N, P, 0, H)
			-> X = 'yes',!
			; X = 'no',!
	).
	

	%shuffle(VesselsList, N, P, 1, H), !.
	


shuffle(VesselsList, N, PeopleCount, C, H):-
	C =< H,
	C1 is C+1,
	pick_two_vessels(A, B),
	
	pour_beer(A, B, VesselsList, UpdatedVesselsList),

			write('Poured beer from '),write(A),write(' to '),write(B),write(',	UpdatedVesselsList = '),writeln(UpdatedVesselsList),

				(is_divided_equally(UpdatedVesselsList, N, PeopleCount)
						->
							%writeln(UpdatedVesselsList),
							true, !
						;
							shuffle(UpdatedVesselsList, N, PeopleCount, C1, H)
				).


pick_two_vessels(A, B):-
	capacity(X,_),
	capacity(Y,_),
	X \= Y,
	A = X,
	B = Y.



%	we have a list of vessels initially with their capacities as capacity(V, C).
%	at each step we have to pour beer from one vessel to another depending on their capacities.
% 	the input vessel list would be like = [12, 0,0,0]
%	pour beer from A to B, should pour from A->B 
%									if A is not empty, 
%											if B is empty, get max capacity of B and empty A into B.
%											if B is not empty, get 
%									and return the updated vessel list with values after pouring the beer from one to another.
%
%	A,B - vessel numbers , positions in VesselsList
%	CRA, CRB - current_capacity of A and B.
%	CAMax, CBMax - max_capacity of A and B.
%	CAA, CAB - available_capacity of A and B.

pour_beer(A, B, VesselsList, UpdatedVesselsList):-
	current_capacity(VesselsList, A, CRA),
	(CRA > 0
		->

			available_capacity(VesselsList, B, CAB),
			(CAB > 0
				->
					(CRA >= CAB
							->
								current_capacity(VesselsList, B, CRB),
								Y is CRB + CAB,
								X is CRA - CAB,
								update_current_capacity(VesselsList, B, Y, VesselsList1),
								update_current_capacity(VesselsList1, A, X, UpdatedVesselsList)

							;	 
								current_capacity(VesselsList, B, CRB),
								Y is CRB + CRA,
								update_current_capacity(VesselsList, B, Y, VesselsList1),
								update_current_capacity(VesselsList1, A, 0, UpdatedVesselsList)
					)

				;	UpdatedVesselsList = VesselsList
			)
		;

			UpdatedVesselsList = VesselsList
	).








current_capacity(VesselsList, V, CR):-
	kth(VesselsList, V, CR).

max_capacity(V, CMax):-
	capacity(V, CMax).

available_capacity(VesselsList, V, CA):-
	max_capacity(V, CMax),
	current_capacity(VesselsList, V, CR),
	CA is CMax-CR.

% update VesselsList
update_current_capacity(VesselsList, V, CR, UpdatedVesselsList):-
	replace(VesselsList, CR, V, UpdatedVesselsList).



is_divided_equally(VesselsList, N, PeopleCount):-
	
	check_less_than_N(VesselsList, N),

	count_zeros(VesselsList, Z),
	length(VesselsList, Length),
	Nonzero is Length-Z,
	Nonzero >= PeopleCount,

	remove_zeros(VesselsList, VesselsList1),

	fill(PeopleCount, [], PersonsList),
	is_divided_equally(VesselsList1, PersonsList, N, PeopleCount).


is_divided_equally(VesselsList, PersonsList, N, PeopleCount):-
	create_people_countList(PeopleCount, PeopleCountList),
	length(VesselsList, M),
	myassign(M, PeopleCountList, PersonComb),
	assign_all_vessels(VesselsList, PersonComb, PersonsList, Updated_personList),
	is_equal_sum(Updated_personList, N),!.
	
		



% myassign returns all the N combinations possible with given list of any size recursively.
% each time it is called a new combination is returned, prolog work to backtrack all those. 

myassign(0,PeopleCountList, []).

myassign(N, PeopleCountList, [H|T]) :-
	N>0,
	member(H,PeopleCountList),
	N1 is N-1,
	myassign(N1, PeopleCountList, T).



create_people_countList(PeopleCount, PeopleCountList):-
	create_1_to_N_list(PeopleCount, PeopleCountList).	


% given an input list which are the repeated person numbers, and an input list of vessel values

% PersonComb = [1,1,2,1,3],
% VesselsList = [4,3,2,1,2],
%  PersonsList = [[],[],[]],

% output = [[4,3,1],[2],[2]],

assign_all_vessels([],[],PersonsList, PersonsList).

assign_all_vessels(VesselsList, PersonComb, PersonsList, X):-
	PersonComb = [H|T1],
	VesselsList = [V|T2],
	kth(PersonsList, H, K),
	replace_element(PersonsList, K, [V|K], Updated_personList),
	assign_all_vessels(T2, T1, Updated_personList, X).




% given a nested list check if each list in it has same sum

is_equal_sum([],N).

is_equal_sum([H|T], N):-
	sum(H, X),
	X =:= N,
	is_equal_sum(T, N).
	



% return a list of given element E with occurences of given size R 

fill(R, E, X):-
	fill(R, E, [], X).

fill(R, E, AccBefore, AccAfter):-
	R >= 0,
	R1 is R-1,
	fill(R1, E, [E|AccBefore], AccAfter).

fill(0, E, AccBefore, AccBefore).

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

delete(H, [H|T], T).

delete(X, [H|T], [H|L]) :-
	delete(X, T, L).

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
	encode1(P, X).

encode1([H|T], X):- encode1([H|T], [], X).

encode1([H|T], AccBefore, AccAfter) :-
	length(H, N),
	H = [H1|T1],
	encode1(T, [[N,H1]|AccBefore], AccAfter).

encode1([], AccBefore, AccAfter) :-
	rev(AccBefore, AccAfter).

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
% return the sum of all the elements in a list

sum(L, X):-
	sum(L, 0, X).

sum([H|T], S, X):-
	S1 is S+H,
	sum(T, S1, X).

sum([],X,X).
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

% replace an element E with R in a given  list

replace_element(L, E, R, X):-
	first_occurence(L, E, P),
	replace(L, R, P, X).
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
% myassign returns all the N combinations possible with given list of any size recursively.
% each time it is called a new combination is returned, prolog work to backtrack all those. 

myassign(0,PeopleCountList, []).

myassign(N, PeopleCountList, [H|T]) :-
	N>0,
	member(H,PeopleCountList),
	N1 is N-1,
	myassign(N1, PeopleCountList, T).

%-----------------------------------------------%-----------------------------------------------%

permute([], []).

permute([X|Xs], Ys) :-
	permute(Xs, Zs),
	delete(X, Ys, Zs).

%-----------------------------------------------%-----------------------------------------------%
count_zeros(L, X):-
	count_zeros(L, 0, X).

count_zeros([],X,X).

count_zeros([H|T], C, X):-
	(H==0
		-> C1 is C+1,
			count_zeros(T, C1, X)
		;
			count_zeros(T, C, X)
	).

%-----------------------------------------------%-----------------------------------------------%
% check if atleast one element is > given element and return false

check_less_than_N([], N).

check_less_than_N([H|T], N):-
	(H > N
		-> false
		;	check_less_than_N(T, N)
	).

%-----------------------------------------------%-----------------------------------------------%
% remove all zeros in given list

remove_zeros(L,X):-
	remove_zeros(L, [], X).

remove_zeros([], X,X).

remove_zeros([H|T], AccBefore, AccAfter):-
	(H==0
		->	remove_zeros(T, AccBefore, AccAfter)
		;	remove_zeros(T, [H|AccBefore], AccAfter)
	).

%-----------------------------------------------%-----------------------------------------------%
% check if all elements are same

are_same(L):-
	L = [H|T],
	are_same(T, H).

are_same([],H).

are_same([T1|T2], H):-
	T1 == H,
	are_same(T2, H).