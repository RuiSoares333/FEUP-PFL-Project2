
/**
 * createLine(+N, +Color, -Line)
*/
createLine(N, M, Line) :- createLine(0, N, M, Line, []).
createLine(_, 0, _, Acc, Acc).
createLine(X, N, M, Line, Acc) :-
    N > 0,
    N1 is N - 1,
    X1 is X + 1,
    append(Acc, [M-X1], Acc1),
    createLine(X1, N1, M, Line, Acc1).

/**
 * createBoard(+N, -Board)
*/
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