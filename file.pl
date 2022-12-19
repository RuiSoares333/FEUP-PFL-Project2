
/**
 * createLine(+N, +Color, -Line)
*/
/*
createLine(N, M, Line) :- createLine(0, N, M, Line, []).
createLine(_, 0, _, Acc, Acc).
createLine(X, N, M, Line, Acc) :-
    N > 0,
    N1 is N - 1,
    X1 is X + 1,
    append(Acc, [M-X1], Acc1),
    createLine(X1, N1, M, Line, Acc1).
*/
/**
 * createBoard(+N, -Board)
*/
/*
createBoard(N, M, Board) :-
    N mod 2 =:= 0, % N must be even
    createBoard(N, Board, [], M, w).

createBoard(_, Acc, Acc, 0, _).
createBoard(N, Board, Acc, Counter, Code) :-
    Counter > 0,
    createLine(N, M, Line),
    append(Acc, [Line], Acc1),
    C1 is Counter - 1,
    createBoard(N, Board, Acc1, C1, Code).
*/

% board(-Board).
% Creates a board container, with uninitialized positions
board(game_board(A,B,C,D,E,F,G,H)):-
	functor(A,l,8), 
	functor(B,l,8),
	functor(C,l,8),
	functor(D,l,8), 
	functor(E,l,8), 
	functor(F,l,8), 
	functor(G,l,8), 
	functor(H,l,8).


% board_initialize_game(-Board).
% Creates a board with the initial state of a game, i.e. the white and the black pieces are placed.
board_initialize_game(game_board(A,B,C,D,E,F,G,H)):-
	board(game_board(A,B,C,D,E,F,G,H)),
	board_initialize_game_odd(A,b),
	board_initialize_game_even(B),
	board_initialize_game_odd(C),
	board_initialize_game_even(D),
	board_initialize_game_odd(E),
	board_initialize_game_even(F),
	board_initialize_game_odd(G),
	board_initialize_game_even(H,w).

% board_initialize_empty_odd(+Line).
% Auxiliary function that initializes a line of the board with alternating black and white spaces.
board_initialize_game_odd(A):-
	arg(1,A,0), arg(2,A,1),
	arg(3,A,0), arg(4,A,1),
	arg(5,A,0), arg(6,A,1),
	arg(7,A,0), arg(8,A,1).

% board_initialize_empty_even(+Line).
% Auxiliary function that initializes a line of the board with alternating black and white spaces.
board_initialize_game_even(A):-
	arg(1,A,1), arg(2,A,0),
	arg(3,A,1), arg(4,A,0),
	arg(5,A,1), arg(6,A,0),
	arg(7,A,1), arg(8,A,0).

% board_initialize_game_odd(+Line,+Player_Symbol).
% Auxiliary function that initializes a line of the board. The player pieces and black spaces are alternaded. 
board_initialize_game_odd(Line,Player):-
	arg(1,Line,0), arg(2,Line,Player),
	arg(3,Line,0), arg(4,Line,Player),
	arg(5,Line,0), arg(6,Line,Player),
	arg(7,Line,0), arg(8,Line,Player).

% board_initialize_game_even(+Line,+Player_Symbol).
% Auxiliary function that initializes a line of the board. The player pieces and black spaces are alternaded.
board_initialize_game_even(Line,Player):-
	arg(1,Line,Player), arg(2,Line,0),
	arg(3,Line,Player), arg(4,Line,0),
	arg(5,Line,Player), arg(6,Line,0),
	arg(7,Line,Player), arg(8,Line,0).


% board_print(+Board).
% Prints the board Board to the console.
/*
board_print(game_board(A,B,C,D,E,F,G,H)):-
	tab(3),print(1), tab(2),print(2), tab(2),
	print(3), tab(2),print(4), tab(2),
	print(5), tab(2),print(6), tab(2),
	print(7), tab(2),print(8), tab(2), nl,
	print(1), tab(2),
	board_print_line(A),
	print(2), tab(2),
	board_print_line(B),
	print(3), tab(2),
	board_print_line(C),
	print(4), tab(2),
	board_print_line(D),
	print(5), tab(2),
	board_print_line(E),
	print(6), tab(2),
	board_print_line(F),
	print(7), tab(2),
	board_print_line(G),
	print(8), tab(2),
	board_print_line(H).
*/
board_print(GB) :-
    board_print_line(GB).

board_print_line([]).

board_print_line( [Head|Tail] ) :-
    board_print_cells(Head),
    write('\n'),
    board_print_line(Tail).

board_print_cells([]).

board_print_cells( [Head|Tail] ) :-
    write(' ~p ', [Head]),
    board_print_cells(Tail).

main:-
    board_initialize_game(Board),
    board_print(Board).
