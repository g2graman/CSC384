/* ----------------------------------------------------------
    CSC384 Assignment 2 

% Surname:
% First Name:
% Student Number: 

  ------------------------------------------------------ */

%do not chabge the following line!
:- ensure_loaded('play.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a piece in this position
%    2 means player two has a piece in this position. 



% given helper: Initial state of the board 
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).
 
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first.
initialize(InitialState,InitialPlyr) :-
	initBoard(InitialState), InitialPlyr is 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player
winner(State, Plyr) :-
	terminal(State), !,
	countNest(State, Plyr, C1),
	countNest(State, 2 - (Plyr - 1), C2), %%other player
	C1 > C2.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :-
	terminal(State),!,
	countNest(State, 1, C1),
	countNest(State, 2, C2),
	C1 == C2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal

terminal(State) :-	
	moves(1, State, X), moves(2, State, Y), !,
	length(X, 0), length(Y, 0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It is used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

addMove(_, _, [PR, PC], X, X) :-
	PR >= 5,
	PC >= 5, !.

addMove(Plyr, State, [PR, PC], PrevMvList, MvList) :-
	(validmove(Plyr, State, [PR, PC]) -> append(PrevMvList, [[PR, PC]], TempList); TempList = PrevMvList),
	NPR is (PR + 1),
	NPC is (PC + 1) mod 6,
	(NPC == 0 -> addMove(Plyr, State, [NPR, NPC], TempList, MvList); addMove(Plyr, State, [PR, NPC], TempList, MvList)).

addMove(_, _, _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%	
moves(Plyr, State, MvList) :-
	addMove(Plyr, State, [0, 0], [], MvList).

moves(_, _, []).
	

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, Move, State, NewState, NextPlyr) :-
	update(Plyr, State, Move, S1, [1,0]),
	update(Plyr, S1, Move, S2, [-1, 0]),
	update(Plyr, S2, Move, S3, [0, 1]),
	update(Plyr, S3, Move, S4, [0, -1]),
	update(Plyr, S4, Move, S5, [-1, -1]),
	update(Plyr, S5, Move, S6, [-1, 1]),
	update(Plyr, S6, Move, S7, [1, -1]),
	update(Plyr, S7, Move, NewState, [1, 1]),
	NextPlyr is 2 - (Plyr - 1).
		

update(Plyr, State, [MoveX, MoveY], NewState, [DR, DC]) :-
	((validate(Plyr, State, [MoveX, MoveY], [DR, DC], 0, Count) -> Count > 0; Count > 0) -> set(State, TempState, [MoveX, MoveY], Plyr), MX is MoveX + DR, MY is MoveY + DC, fixBracket(Plyr, TempState, [MX, MY], [DR, DC], NewState, Count); NewState = State).

update(_, State, _, State, _).

fixBracket(Plyr, State, [SR, SC], [DR, DC], NewState, LeftToChange) :-
	(LeftToChange > 0 -> set(State, TempState, [SR, SC], Plyr), NSR is SR + DR, NSC is SC + DC, NewLeft is LeftToChange - 1, fixBracket(Plyr, TempState, [NSR, NSC], [DR, DC], NewState, NewLeft); set(State, NewState, [SR, SC], Plyr)).

fixBracket(_, State, _, _, State, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%%  - true if Proposed move by Plyr is valid at State (if Plyr brackets at least 1 piece in any direction)
%%
validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [1, 0], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [-1, 0], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [0, 1], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [0, -1], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [-1, -1], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [-1, 1], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [1, -1], 0, Count) -> Count > 0; Count > 0).

validmove(Plyr,State,[PR, PC]) :-
	get(State, [PR, PC], '.'),
	(validate(Plyr, State, [PR, PC], [1, 1], 0, Count) -> Count > 0; Count > 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%validate(Plyr, State, Proposed, Direction, Count)%%%%%%%%%%%%%%%%%%%
%% 
%% validate(Plyr, State, Proposed, Direction, Count)
%% -Always returns true and upon completion sets Count to the number of bracketed pieces with respect to the
%% direction vector [DR, DC] from the initial proposed move.

validate(Plyr, State, [PR, PC], [DR, DC], LC, Count) :-
	Row is PR + DR,
	Col is PC + DC,
	Other is 2 - (Plyr - 1),
	(get(State, [Row, Col], Other) -> TC is LC + 1, validate(Plyr, State, [Row, Col], [DR, DC], TC, Count); get(State, [Row, Col], Plyr), Count is LC).

validate(_ ,_ ,_ , _, 0, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State, Val) :-
	winner(State, 1), Val is 1, !.

h(State, Val) :-
	winner(State, 2), Val is -1, !.

h(State, Val) :-
	(\+(terminal(State)); tie(State)), Val is 0. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position row R column C
% set(Board, NewBoard, [R, C], Value):
%    : set Value at row R column C in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [0,5], the lower left
% hand corner has index [5,0], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [3,2], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [4,2], 1), set(NB1, NB2, [3,2], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% row R column C (indexing starts at 0).
get( Board, [R, C], Value) :- 
	nth0( R, Board, Row), 
	nth0( C, Row, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [0, C], Value)
    :- setInList(Row, NewRow, C, Value). 

set( [Row|RestRows], [Row|NewRestRows], [R, C], Value) :- 
	R > 0, 
	R1 is R-1, 
	set( RestRows, NewRestRows, [R1, C], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 

count([], _, 0).
count([Element | RestList], Plyr, C) :-
	count(RestList, Plyr, C1), 
	(Element == Plyr -> C is C1 + 1;  C is C1).

countNest([], _, 0).
countNest([FirstList | SecondList], Plyr, C) :-
	count(FirstList, Plyr, C1), count(SecondList, Plyr, C2), 
	C is C1 + C2.
