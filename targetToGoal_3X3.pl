test([A,B,C,D,E,F,G,H,I]):-go([A,B,C,D,E,F,G,H,I],[1,2,3,8,0,4,7,6,5]).
move(S,Snew):-
  	right(S,Snew).

right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R3>0,R6>0,R9>0,
  	blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9], 0),
  	Z is N+1,
 	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	left(S,Snew).

left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R1>0,R4>0,R7>0,
  	blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N-1,
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	down(S,Snew).

down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R7>0,R8>0,R9>0,
  	blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N+3,
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	up(S,Snew).

up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R1>0,
  	R2>0,
  	R3>0,
  	blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N-3,
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

substitute(_, [], _, []):-!.
substitute(X, [X|T], Y, [Y|T1]):-
	substitute(X, T, Y, T1),!.

substitute(X, [H|T], Y, [H|T1]):-
	substitute(X, T, Y, T1).

go(Start,Goal):-
		path([[Start,null]],[],Goal).

path([],,):-
		write('No solution'),nl,!.

path([[Goal,Parent] | _], Closed, Goal):-
		write('A solution is found'), nl ,
		printsolution([Goal,Parent],Closed),!.

path(Open, Closed, Goal):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent] | Closed], Goal).

getchildren(State, Open ,Closed , Children):-
		bagof(X, moves( State, Open, Closed, X), Children), ! .
getchildren(,,_, []).

addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

removeFromOpen([State|RestOpen], State, RestOpen).

moves( State, Open, Closed,[Next,State]):-
		move(State,Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

print([R1,R2,R3,R4,R5,R6,R7,R8,R9]):-
	write(R1-R2-R3),nl,
	write(R4-R5-R6),nl,
	write(R7-R8-R9),nl.

printsolution([State, null],_):-
		print(State),nl.
printsolution([State, Parent], Closed):-
		member([Parent, GrandParent], Closed),
		printsolution([Parent, GrandParent], Closed),
		print(State), nl.
