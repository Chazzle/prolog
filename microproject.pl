/* Microproject */

?- length([1, 3, 6], What).


addrow(Z, [X|Y], [X|R]) :- addrow(Z, Y, R).
addrow(X, [], X).

blocks([[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]).

get_blocks([]).
get_blocks([X|Y]):-
	write(X), nl,
	get_blocks(Y).
get_blocks :- 
	blocks(X),
	get_blocks(X).
	
	
find_col([]).
find_col(X) :-
	blocks(Y),
	find_col(X,Y).
find_col(X, [Y|Z]) :-
	
	find_col(X, Z).

	
point(1,1). point(1,2). point(1,3). point(1,4). point(1,5). point(1,6). point(1,7). point(1,8). point(1,9). point(1,10).  
point(2,1). point(2,2). point(2,3). point(2,4). point(2,5). point(2,6). point(2,7). point(2,8). point(2,9). point(2,10). 
point(3,1). point(3,2). point(3,3). point(3,4). point(3,5). point(3,6). point(3,7). point(3,8). point(3,9). point(3,10). 
point(4,1). point(4,2). point(4,3). point(4,4). point(4,5). point(4,6). point(4,7). point(4,8). point(4,9). point(4,10). 
point(5,1). point(5,2). point(5,3). point(5,4). point(5,5). point(5,6). point(5,7). point(5,8). point(5,9). point(5,10). 
point(6,1). point(6,2). point(6,3). point(6,4). point(6,5). point(6,6). point(6,7). point(6,8). point(6,9). point(6,10). 
point(7,1). point(7,2). point(7,3). point(7,4). point(7,5). point(7,6). point(7,7). point(7,8). point(7,9). point(7,10). 
point(8,1). point(8,2). point(8,3). point(8,4). point(8,5). point(8,6). point(8,7). point(8,8). point(8,9). point(8,10). 
point(9,1). point(9,2). point(9,3). point(9,4). point(9,5). point(9,6). point(9,7). point(9,8). point(9,9). point(9,10).
point(10,1). point(10,2). point(10,3). point(10,4). point(10,5). point(10,6). point(10,7). point(10,8). point(10,9). point(10,10). 
point(11,1). point(11,2). point(11,3). point(11,4). point(11,5). point(11,6). point(11,7). point(11,8). point(11,9). point(11,10). 
point(12,1). point(12,2). point(12,3). point(12,4). point(12,5). point(12,6). point(12,7). point(12,8). point(12,9). point(12,10).  


edge(Point1X, Point1Y, Point2X, Point2Y) :-
	point(Point1X, Point1Y),
	point(Point2X, Point2Y),
	(Point1X =:= Point2X ; Point1Y =:= Point2Y),
	point(Point1X, Point1Y) \== point(Point2X, Point2Y),
	abs((Point1X + Point1Y) - (Point2X + Point2Y)) < 2.
	
obstacle(X,Y,Z,R) :-
	X1 is X + 1,
	XY is X + Y,
	XZ is 10 - Z,
	XZZ is 10 - Z + 1,
	point(X1,10),
	point(XY, XZ),
	findall([A,B], (between(X1,XY,A),between(XZZ,10,B)),R).
	
add_obstacles([],[]).
add_obstacles([[X,Y,Z]|S],R) :-
	add_obstacles(S, L),
	obstacle(X,Y,Z,O),
	append(O,L,R).
	
walkways(Y,Z,R) :-
	between(2,9,X),
	XY is X + Y,
	point(X,1),
	point(XY, Z),
	findall([A,B], (between(X,XY,A),between(1,Z,B)),R).
add_walkway(R) :-
	walkways(2,6,R).
	
add_walkway2(X,Walkway) :-
	between(2,10,X),
	Width is X + 1,
	Length is 6,
	findall([A,B], (between(X, Width, A),between(1, Length, B)),Walkway).
	
reflect(1, \, 4).
reflect(1, /, 2). 
%reflect(2, \, 3).
reflect(2, /, 1).
reflect(3, \, 2).
reflect(3, /, 4).
reflect(4, \, 1).
%reflect(4, /, 3).
	
	
get_beam([X,Y], 1, R):-
	findall([A,B], (between(X,12,A), between(Y,Y,B)), R).
get_beam([X,Y], 2, R):-
	findall([A,B], (between(X,X,A), between(Y,10,B)), R).
get_beam([X,Y], 3, R):-
	findall([A,B], (between(1,X,A), between(Y,Y,B)), L),
	reverse(L, R).
get_beam([X,Y], 4, R):-
	findall([A,B], (between(X,X,A), between(1,Y,B)), L),
	reverse(L,R).


prefix(X, L, [X|L]).
	
	
	
	
reflect_beam([X,Y],[[X,Y]|R], ND, NB) :-
	get_beam([X,Y], ND, NB).
reflect_beam([X,Y], [F|R], ND, NB) :-
	reflect_beam([X,Y], R, ND, B),
	prefix(F, B, NB).
	
direction(1).
direction(2).
direction(3).
direction(4).
angle(\).
angle(/).
%/

mirror([X,Y], 2, /) :-
	point(X,Y),
	direction(2),
	angle(/),
	reflect(2, /, ND).
%/
mirror([X,Y], 4, \) :-
	point(X,Y),
	direction(4),
	angle(\).
mirror([X,Y], 1, Angle) :-
	point(X,Y),
	direction(1),
	angle(Angle).
mirror([X,Y], 3, Angle) :-
	point(X,Y),
	direction(3),
	angle(Angle).
	
mirror([X, Y, Angle], Direction, Beam, ND, NB):-
	reflect(Direction, Angle, ND),
	reflect_beam([X,Y], Beam, ND, NB).


	
add_mirrors([], ND, NB, NB2) :-
	NB2 = NB.
add_mirrors([[X,Y,A]|S], Direction, Beam, NB2) :-
	mirror([X,Y,A], Direction, Beam, ND, NB),
	add_mirrors(S, ND, NB, NB2).
	
	
laser(Start, Direction, R) :- 
	point(1,Start),
	point(12,Start),
	findall([A,B], (between(1,12,A), between(Start,Start,B)),R).
laser(Start, Mirrors, Direction, R, NB2) :-
	point(1,Start),
	point(12,Start),
	findall([A,B], (between(1,12,A), between(Start,Start,B)),Beam),
	add_mirrors(Mirrors, Direction, Beam, NB2).
	
%contains(X, [Y|Z]):-
get_last(X, [Y|X]) :- length(X,1).
get_last(X, [Y|Z]) :- get_last(X, Z).

get_first([X|_], First) :- First = X.
remove_first([X|Y], Rest) :- Rest = Y. 

get_item([X|Y], X).
get_Y([X|Y], Y).
	
hits_obstacle([O|OO], [B|BB]) :-
	member(O, [B|BB]).
hits_obstacle([O|OO], [B|BB]) :-
	hits_obstacle(OO, [B|BB]).
	
pairUp(List, RList) :-
	pairUp(List, [], Ans),
	reverse(Ans, RList),
	!.
pairUp(List, ListAns, ListAns) :-
	length(List, Len),
	Len < 2.
pairUp([H|T], List, Ans) :-
	get_first(T, Second),
	remove_first(T, NewT),
	append([[H,Second]], List, NewList),
	pairUp(NewT, NewList, Ans).
	
new_mirrors([X,Y,Angle], Direction, [A,B], All, Total, Allr, Height) :-
	Total < 4,
	reflect(Direction, Angle, ND),
	mirror([A,B], ND, NA),
	\+  (A = X, B=Y),
	((Direction = 1, Y = B) ; (X = A)),
	%append([X,Y],[A,B],Allr),
	NT = Total + 1,
	write(X),write(Y),write(Angle),write("\n"),
	new_mirrors([A,B,NA],  ND, [A2,B2], Allz, NT, Allx, Height),
	append([X,Y], Allx, Allr),
	All = Allr.
	%print(XX).
new_mirrors([X,Y,Angle], Direction, [A,B], All, Total, Allr, Height) :-
	Y = Height,
	write(X),write(Y),
	append([X,Y], [], Allr).
	
	
%pair_last_ele([H|[]], New_Ele, New_List) :- 
	
pair_last_ele([H|T], New_Ele, New_List) :- pair_last_ele(T, New_Ele, New_List).
	
%new_mirrors2(
	
valid_mirrors([X,Y,Angle], All) :-
	laser(X, 1, Beam),
	mirror([X,Y], 1, Angle),
	%new_mirrors([X,Y,Angle], 1, [A,B], All, 1, Allr, X),
	%pairUp(All, Ans),
	length(All, XX),
	XX = 8,
	print(XX).
	

test_iter(TotalNumbers, CurrentNumbers, Numbers) :-
	TotalNumbers < 8,
	TotalNumbers2 is TotalNumbers + 1,
	append(CurrentNumbers, [TotalNumbers2], CurrentNumbers2),
	test_iter(TotalNumbers2, CurrentNumbers2, Numbers).
test_iter(TotalNumbers, CurrentNumbers, Numbers) :-
	Numbers = CurrentNumbers,
	((TotalNumbers = 4) ; (TotalNumbers = 6) ; (TotalNumbers = 8)).
test_iter(Numbers) :-
	TotalNumbers = 0,
	append([TotalNumbers], [], CurrentNumbers),
	test_iter(TotalNumbers, CurrentNumbers, Numbers).
	
last_ele([H|[]], Last) :-
	Last = [H].
last_ele([H|T], Last) :- last_ele(T, Last).
	
		
get_exit_beam(Beam, Exit) :-
	last_ele(Beam, Exit2),
	flatten(Exit2,Exit).

add_beam_mirror(Beam, Direction, [X,Y,Angle], NewBeam, NewDir, CurrentMirrors,FX,FY,Obstacles,Walkway) :-
	
	member([X,Y], Beam),
	\+ member([X,Y,_], CurrentMirrors),
	mirror([X, Y, Angle], Direction, Beam, NewDir, NewBeam),
	((Direction == 1 -> X > FX);X=FX),
	\+ member([X,Y], Obstacles).
	%\+ hits_obstacle(Walkway,NewBeam),
	%\+ hits_obstacle(Obstacles,NewBeam).
	

add_all_mirrors(Beam, Direction, Height, Count, CurrentMirrors, AllMirrors, FX, FY, Obstacles,Walkway, FinalBeam) :-
	((Count=4);(Count=6);(Count=8)),
	add_walkway2(Z,Walkway),
	\+hits_obstacle(Obstacles, Walkway),
	\+hits_obstacle(Beam, Walkway),
	get_exit_beam(Beam, [EX, EY]),
	EX = 12,
	EY = Height,
	FinalBeam = Beam,
	AllMirrors = CurrentMirrors.	
add_all_mirrors(Beam, Direction, Height, Count, CurrentMirrors, AllMirrors, FX, FY, Obstacles,Walkway, FinalBeam) :-
	Count < 6,	
	add_beam_mirror(Beam, Direction, [X,Y,Angle], NewBeam, NewDir, CurrentMirrors,FX, FY, Obstacles,Walkway),
	NewCount is Count + 1,
	append(CurrentMirrors, [[X,Y,Angle]], CurrentMirrors2),
	add_all_mirrors(NewBeam, NewDir, Height, NewCount, CurrentMirrors2, AllMirrors, X, Y, Obstacles,Walkway, FinalBeam).

	
add_all_mirrors(Beam, Direction, Height, AllMirrors, Obstacles, Walkway,FinalBeam) :-
	Count = 0,
	add_beam_mirror(Beam, Direction, [X,Y,Angle], NewBeam, NewDir, [], 0, 0, Obstacles, Walkway),
	NewCount is Count + 1,
	append([[X,Y,Angle]], [], CurrentMirrors),
	add_all_mirrors(NewBeam, NewDir, Height, NewCount, CurrentMirrors, AllMirrors, X, Y, Obstacles,Walkway, FinalBeam),
	\+ hits_obstacle(FinalBeam, Obstacles).
	
	
valid_mirrors2(Beam, AllMirrors) :-
	add_all_mirrors(Beam).

	
test_path(Height, O, [B|BB]) :-
	\+ hits_obstacle(O, [B|BB]),
	get_last(X,[B|BB]),
	get_item(X, Item),
	get_Y(Item, Y),
	[Height] == Y.

	
	
place_mirrors(Height, Obstacles, Mirrors) :-
	laser(Height, 1, Beam),
	add_obstacles(Obstacles, O),
	add_all_mirrors(Beam, 1, Height, Mirrors, Obstacles, Walkway, FinalBeam).
	
	
	
	


all_but_last([H|[]], Final_List) :-
	Final_List = [].
all_but_last([H|T], Final_List) :-
	append([H], [], Temp_List),
	all_but_last(T, Temp_List, Final_List).
all_but_last([H|[]], Temp_List, Final_List) :-
	Final_List = Temp_List.
all_but_last([H|T], Temp_List, Final_List) :-
	append(Temp_List, [H], NTemp_List),
	all_but_last(T, NTemp_List, Final_List).
	
traverse(H,[],Pair_list) :-
	PairsX = Pair_list,
	append(PairsX, H, Pair_list).
traverse(H, [New|List], Pair_list) :- 
	last_ele([New|List], Last),
	length([Last], Arity),
	check_pair(H, Arity, [New|List], Pair_list).

get_ele_length([H|T], Len) :-
	length(H, Len).
get_ele_length([H|T], Len) :-
	length([H], Len).
