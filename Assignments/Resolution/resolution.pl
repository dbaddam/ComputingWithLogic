
% add all the given clauses into a list, elements index is its id.

% given L = [[1,[a_1,a_2],0],
%			 [2,[neg(a_2),b_2,b_3],0],
%			 [3,[neg(b_2),b_3],0],
%			 [4,[neg(b_3)],0],
%			 [5,[a_1],0]
%			]
%
% 0-indicates that this clause has not been visited before and can be used for resolution.
% Perform resolution on any two random clauses, add the result to the list and 
% change the 0s of the selected 2 clauses to 1-making them visisted so that they wont be visted again in future



resolution(X1):-
	load_dyn(X1),

	findall(X,myClause(_,X),Z),
	create_list(Z,Z1),

	get_query_list(Q),

	Q=[H|_],
	(is_list(H)
		->	append(Z1,Q,Z2)
		;	append(Z1,[Q],Z2)
	),

	writeln(''),
	writeln(''),

	(res(Z2)
		-> %writeln('resolution(success).')
			true
		;writeln('resolution(fail).')
	).




get_query_list(Q):-
	myQuery(N,X),
	(\+is_atom(X)
		->
			X=or(A,B),
			orList(A,B,L),
			get_neg_list(L,Q1),
			get_and_list(Q1,N,Q2)
			%Q=[N,Q1,0]
		;
			% must be neg(X) or X as clause.

			(X=neg(A)
				->
					Q2=[N,[A],0]
				;
					Q2=[N,[neg(X)],0]
			)
	),
	(is_list(Q2)->Q=Q2;Q=[Q2]).



get_and_list(Q1,N,Q2):-
	get_and_list(Q1,N,[],Q2).

get_and_list([],_,AccB,AccA):-reverse(AccB,AccA).

get_and_list([H|T],N,AccB,AccA):-
	N1 is N+1,
	get_and_list(T,N1,[[N,[H],0]|AccB],AccA).




get_neg_list(L,X):-
	get_neg_list(L,[],X).

get_neg_list([],AccB,AccA):-reverse(AccB,AccA).

get_neg_list([H|T],AccB,AccA):-
	(H=neg(A)
		->
			H=neg(A),
			get_neg_list(T,[A|AccB],AccA)
		;
			get_neg_list(T,[neg(H)|AccB],AccA)
	).


create_list(L,X):-
	create_list(L,0,[],X).

create_list([],_,AccB,AccA):-reverse(AccB,AccA).

create_list([H|T],N,AccB,AccA):-
	N1 is N+1,
	(\+is_atom(H)
		->
			H=or(A,B),
			orList(A,B,L),
			create_list(T,N1,[[N1,L,0]|AccB],AccA)
		;
			% must be neg(X) clause.
			create_list(T,N1,[[N1,[H],0]|AccB],AccA)
	).


res(L):-
	length(L,N),
	res(L,N).

res([]):-!,fail.

res(L,N):-
	\+is_empty(L),

	member(X,L),
	member(Y,L),
	X\=Y,
	\+is_visited(X),
	\+is_visited(Y),

	X=[N1,L1,_],
	Y=[N2,L2,_],

	%write('Appyling resolution for '),write(N1),write(',  '),writeln(N2),
	resolution_lists(L1,L2,L3),
	
	Len1 is N+1,
	append(L,[[Len1,L3,0]],L4),

	delete(X,L4,Z11),
	delete(Y,Z11,Z22),

	(\+is_empty(L3)
		->
			
			res(Z22, Len1),
			write('resolution('),write(N1),write(','),write(N2),write(','),print(L3),write(','),write(Len1),writeln(').')

		; 
			writeln('resolution(success).'),
			write('resolution('),write(N1),write(','),write(N2),write(','),write('empty'),write(','),write(Len1),writeln(').')
	).





is_empty(L3):-
	length(L3,N),
	N==0.

is_visited(X):-
	X=[N,L,V],
	V==1.

is_atom(X):-
	(atom(X);integer(X)
		-> true
		;	
			X=neg(Y)
	).

orList(X,Y,L):-
	is_atom(X),is_atom(Y),
	L=[X,Y].

orList(X,Y,L):-
	is_atom(X),\+is_atom(Y),
	Y=or(A,B),
	orList(A,B,L1),
	L=[X|L1].

orList(X,Y,L):-
	\+is_atom(X),is_atom(Y),
	X=or(A,B),
	orList(A,B,L1),
	L=[Y|L1].

orList(X,Y,L):-
	\+is_atom(X),\+is_atom(Y),
	X=or(A1,B1),
	orList(A1,B1,L1),
	Y=or(A2,B2),
	orList(A2,B2,L2),
	append(L1,L2,L).


% Given two lists L1 = [X,Y,Z] and L2 = [neg(X),Y,neg(Z)], output the L = [Y] after resolution
% NOTE: before passing here, we should make sure that both L1 and L2 are unique.

resolution_lists(L1,L2,L):-
	resolution_lists(L1,L2,[],L3),
	length(L1,N1),
	length(L2,N2),
	length(L3,N3),
	N4 is N1+N2,
	(N3 < N4
		->	L=L3
		;	fail
	).

resolution_lists([],L2,AccB,AccA):-
	append(L2,AccB,AccA).

resolution_lists([H|T],L2,AccB,AccA):-
	(\+atom(H)
		->
			H=neg(X),
			(member(X,L2)
		        ->
					delete(X,L2,L3),
					resolution_lists(T,L3,AccB,AccA)
				;
					(member(H,L2)
						->	%fail
							resolution_lists(T,L2,AccB,AccA)

						;	resolution_lists(T,L2,[H|AccB],AccA)
					)
					
				
			)


		;
			(member(neg(H),L2)
		        ->
					delete(neg(H),L2,L3),
					resolution_lists(T,L3,AccB,AccA)
				;
					(member(H,L2)
						->
							%fail
							resolution_lists(T,L2,AccB,AccA)
						;
							resolution_lists(T,L2,[H|AccB],AccA)
					)
			)

	).

print(L3):-
	length(L3,N),
	(N==1
		-> 
			L3=[H],
			write(H)
		;
			write('or('),print1(L3),write(')')
	).

print1([]).

print1([H|T]):-
	write(H),
	(\+is_empty(T)
		->write(','),print1(T)
		;print1(T)

	).
%-----------------------------------------------%-----------------------------------------------%
length([],0).
%length([[]],0).

length([H|T],X):-
	length(T, N),
	X is N+1.
%-----------------------------------------------%-----------------------------------------------%	
% reverse a list using accumulater

reverse(L,P) :- reverse(L, [], P).

reverse([], AccBefore, AccBefore).

reverse([H|T], AccBefore, AccAfter) :-
	reverse(T, [H|AccBefore], AccAfter).

%-----------------------------------------------%-----------------------------------------------%
% member says if that element is present in the given list or not

member(X, [X|T]).

member(X, [H|T]) :-
	member(X, T).

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