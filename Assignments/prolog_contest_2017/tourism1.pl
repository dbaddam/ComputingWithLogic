 %people(4). locations(4). preferences(8). order(1, 1, 2). order(1, 2, 3). order(2, 4, 3). order(2, 3, 2). order(3, 2, 4). order(3, 4, 1). order(4, 1, 3). order(4, 3, 4).



new_order(P, X, Y) :-
	order(P, X, Y).
new_order(P, X, Y):-
	order(P, X, Z),
	new_order(P, Z, Y).
		

violations(K):-
	people(P),
	locations(N),

	create_1_to_N_list(N, L),
	
	findall(V, get_min_violations(L, P, V),X),
	min(X,M),
	
	K=M.
	

get_min_violations(L, P, V):-
	permute(L, L1),
	count_violations(L1,P,0,V).



% given a list of tourism location order decided by guide, count number of violations of ALL persons.


count_violations(L,N,V, X):-
	(N =< 0
		->
			X=V
		;
			count_violations_person(L, N, V, X1),
			N1 is N-1,
			count_violations(L, N1, X1, X)
	).


% given a list of tourism location order decided by guide, count number of violations of given person.

count_violations_person(L, P, V, X):-
	L = [H|T],
	count_violations_person(H, T, P, 0, X1),
	V1 is V+X1,
	count_violations_person(T, P, V1, X).

count_violations_person([],P,V,V).




count_violations_person(H, [T1|T2], P, V, X):-
%	write('Checking violation for Person = '),write(P),write('---'),write(H),write(','),writeln(T1),
	(new_order(P, T1, H)
			->
			%	writeln('Violation'),
				V1 is V+1,
				count_violations_person(H, T2, P, V1, X)
			;
			%	writeln('Not a Violation '),
				count_violations_person(H, T2, P, V, X)


	).

count_violations_person(H, [], P, V, V).



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

lowest(A, B, X):-
	(A =< B
		-> X = A
		;  X = B).

min([H|T], X) :- min([H|T], H, X).

min([H|T], V, X):-
	lowest(H, V, Z),
	min(T, Z, X).

min([], V, X):- X = V.
