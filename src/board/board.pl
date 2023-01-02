/**
 * switchColor(?Player1, ?Player2)
*/
switchColor(r, b).
switchColor(b, r).



jumper2Skipper(rJ, rS).
jumper2Skipper(rS, rS).
jumper2Skipper(bJ, bS).
jumper2Skipper(bS, bS).

/**
 * color(+Cell, -Elem)
 *
 * Gets the color of the cell
*/
color(A-_, A).

/**
 * state(+Cell, -Elem) 
 *
 * Gets the state of the cell (empty or either of the players)
*/
state(rJ, r).
state(rS, r).
state(bJ, b).
state(bS, b).

emptyState(e, _).

jumperState(rJ, r).
jumperState(bJ, b).


enemyState(rJ, b).
enemyState(rS, b).
enemyState(bJ, r).
enemyState(bS, r).

/**
 * createLine(+N, +Color, -Line)
*/
createLine(M, Color, Line) :- createLine(M, M, Color, Line, []).
createLine(_, 0, _, Acc, Acc).

createLine(M, M, r, Line, Acc) :-
    M1 is M - 1,
    append(Acc, [rJ], Acc1),
    createLine(M, M1, r, Line, Acc1).

createLine(M, 1, b, Line, Acc) :-
    append(Acc, [bJ], Acc1),
    createLine(M, 0, r, Line, Acc1).

createLine(M, C, Color, Line, Acc) :-
    C1 is C - 1,
    append(Acc, [e], Acc1),
    createLine(M, C1, Color, Line, Acc1).

/**
 * createBoard(+N, +M, -Board)
*/
createBoard(N, M, Board) :-
    createBoard(N, M, Board, [], N, r).

createBoard(_, _, Acc, Acc, 0, _).
createBoard(N, M, Board, Acc, Counter, Color) :-
    createLine(M, Color, Line),
    append(Acc, [Line], Acc1),
    C1 is Counter - 1,
    switchColor(Color, NextColor),
    createBoard(N, M, Board, Acc1, C1, NextColor).

/**
 * getCell(+Board, +X, +Y, -Cell)
*/
getCell(Board, X, Y, Cell) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Cell).

/**
 * replaceCell(+Board, +X, +Y, -NewCell, -NewBoard)
*/

replaceCell(Board, X, Y, NewCell, NewBoard) :-
    nth0(Y, Board, Line),
    replace(X, Line, NewCell, NewLine),
    replace(Y, Board, NewLine, NewBoard).

/**
 * boardDimensions(+Board, -LineNumber, -ColumnNumber)
*/
boardDimensions(Board, LineNumber, ColumnNumber) :-
    length(Board, LineNumber),
    nth0(0, Board, Line),
    length(Line, ColumnNumber).

validPlayer(r).
validPlayer(b).