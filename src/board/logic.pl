/**
 * validateShiftStone(+GameState, +Pos, +NewPos)
 *
 * Verifies if the movement from Pos to NewPos is possible
 */
validateShiftStone((Board, Player), (X, Y), (X, NewY)) :-
    getCell(Board, X, Y, OwnCell) ,
    jumperState(OwnCell, Player),

    getCell(Board, X, NewY, OtherCell) ,
    enemyState(OtherCell, Player),

    abs(NewY - Y) =:= 1,
    NewPos is NewY + NewY - Y,
    
    (getCell(Board, X, NewPos, e) ; \+ checkInBound(Board, (X, NewPos))).


validateShiftStone((Board, Player), (X, Y), (NewX, Y)) :-
    getCell(Board, X, Y, Cell),
    state(Cell, Player),

    (getCell(Board, NewX, Y, e) ; \+ checkInBound(Board, (NewX, Y))),
    verifyShift(Player, X, NewX).

/**
 * verifyShit(+Player, +X, +NewX)
 *
 * Verifies if the move is being made in the correct direction
*/

verifyShift(b, X, NewX) :-
    number(NewX),
    Val is NewX - X,
    Val =:= -1. % Black moves left so value has to be negative

verifyShift(r, X, NewX) :-
    number(NewX),
    Val is NewX - X,
    Val =:= 1. % Red moves right so value has to be positive


/**
 * shiftStone(+GameState, +Pos, +NewPos, -NewGameState)
 *
 * Moves the piece in Pos to NewPos
 */
shiftStone((Board, Player), (X, Y), (X, NewY), (NewBoard, Player)) :-
    YPos is Y + (NewY - Y) * 2,
    checkInBound(Board, (X, YPos)),

    getCell(Board, X, NewY, JumperCell),                     % get the cell on the new position coordinates
    jumper2Skipper(JumperCell, SkipperCell),                 % change other player's cell to skipper
    replaceCell(Board, X, NewY, SkipperCell, MidBoard1),
    
    getCell(MidBoard1, X, Y, PlayerCell),
    replaceCell(MidBoard1, X, YPos, PlayerCell, MidBoard2),

    replaceCell(MidBoard2, X, Y, e, NewBoard).


shiftStone((Board, Player), (X, Y), (X, NewY), (NewBoard, Player)) :-
    getCell(Board, X, NewY, JumperCell),                     % get the cell on the new position coordinates
    jumper2Skipper(JumperCell, SkipperCell),                 % change other player's cell to skipper
    replaceCell(Board, X, NewY, SkipperCell, MidBoard1),

    replaceCell(MidBoard1, X, Y, e, NewBoard).


shiftStone((Board, Player), (X, Y), (NewX, Y), (NewBoard, Player)) :-
    checkInBound(Board, (NewX, Y)),

    getCell(Board, X, Y, PlayerCell),
    
    replaceCell(Board, X, Y, e, MidBoard1),
    replaceCell(MidBoard1, NewX, Y, PlayerCell, NewBoard).


shiftStone((Board, Player), (X, Y), (NewX, Y), (NewBoard, Player)) :-
    replaceCell(Board, X, Y, e, NewBoard). % Last case, if all others fail it means that the piece left the board

/**
 * checkInBound(+Board, +Pos)
 *
 * Checks if Pos is inside the Board
 */

checkInBound(Board, (X, Y)) :-
    boardDimensions(Board, LineNumber, ColumnNumber),
    number(X), number(Y),
    X >= 0,
    X < ColumnNumber,
    Y >= 0,
    Y < LineNumber.

/**
 * findMoves(+GameState, +Move, -NewGameState)
 *
 * Used by validMoves to find the possible moves for a given GameState
 */

findMoves(GameState, (X, Y)-(X1, Y1), NewGameState) :-
    validateShiftStone(GameState, (X, Y), (X1, Y1)),
    shiftStone(GameState, (X, Y), (X1, Y1), NewGameState).

/**
 * move(+GameState, +Move, -NewGameState)
 *
 * Executes a move
 */

move(GameState, (X, Y)-(X1, Y1), NewGameState) :-
    validateShiftStone(GameState, (X, Y), (X1, Y1)),
    shiftStone(GameState, (X, Y), (X1, Y1), NewGameState).

move((Board, Player), (X, Y)-(X, Y), (NewBoard, Player)) :- % If the movement is staying in the same position it means that it left the board
    getCell(Board, X, Y, Cell),
    state(Cell, Player),
    replaceCell(Board, X, Y, e, NewBoard).

/**
 * validMoves(+GameState, -Moves)
 *
 * Generates all the possible Moves for a given GameState
*/

validMoves((Board, Player), Moves):-
    setof(Move, NewState^(findMoves((Board, Player), Move, NewState)), Moves).

validMoves((Board, r), Moves) :- % Checks if red Jumpers are leaving the board
    boardDimensions(Board, LineNumber, ColumnNumber),
    C is ColumnNumber - 1,
    findall((C, Y), getCell(Board, C, Y, rJ), Cells),
    random_select((X1, Y1), Cells, _Rest),
    append([], [(X1, Y1)-(X1, Y1)], Moves).

validMoves((Board, r), Moves) :- % Checks if red Slippers are leaving the board
    boardDimensions(Board, LineNumber, ColumnNumber),
    C is ColumnNumber - 1,
    findall((C, Y), getCell(Board, C, Y, rS), Cells),
    random_select((X1, Y1), Cells, _Rest),
    append([], [(X1, Y1)-(X1, Y1)], Moves).

validMoves((Board, b), Moves) :- % Checks if black Jumpers are leaving the board
    findall((0, Y), getCell(Board, 0, Y, bJ), Cells), 
    random_select((X1, Y1), Cells, _Rest),
    append([], [(X1, Y1)-(X1, Y1)], Moves).

validMoves((Board, b), Moves) :-% Checks if black Slippers are leaving the board
    findall((0, Y), getCell(Board, 0, Y, bS), Cells),
    random_select((X1, Y1), Cells, _Rest),
    append([], [(X1, Y1)-(X1, Y1)], Moves).

/**
 * checkWin(+Vectors, +Player)
 *
 * Check if the player won, by analyzing the list of vectors
 */
checkWin([], _) :- false.

checkWin([Vector | T], r) :- 
    sublist(Vector, [rJ], _) ; sublist(Vector, [rS], _) , ! .

checkWin([Vector | T], b) :- 
    sublist(Vector, [bJ], _) ; sublist(Vector, [bS], _), !.

checkWin([Vector | T], Player) :-
    checkWin(T, Player).
    
/**
 * getWinCondition(+Player, +NumPieces, -WinCondition)
 *
 * Generates a win condition (list of player-owned cells) with the given number of pieces
 */
getWinCondition(Player, NumPieces, WinCondition) :- getWinCondition(Player, NumPieces, WinCondition, []).
getWinCondition(_, 0, WinCondition, WinCondition) :- !.
getWinCondition(Player, NumPieces, WinCondition, Acc) :-
    append(Acc, [Player], Acc1),
    NumPieces1 is NumPieces - 1,
    getWinCondition(Player, NumPieces1, WinCondition, Acc1).

/**
 * gameOver(+GameState, -Winner)
 */
gameOver((Board,_), Player) :-
    validPlayer(Player),
    \+ checkWin(Board, Player).  % Rows

/**
 * initialState(+Size, -GameState)
 */
initialState(SizeN, SizeM, (Board, FirstPlayer)) :- % Black always goes first
    random(0, 2, R),
    getFirstPlayer(R, FirstPlayer),
    createBoard(SizeN, SizeM, Board).

getFirstPlayer(0, r).
getFirstPlayer(1, b).

/**
 * countPieces(+Board, +Cell, -Num)
 *
 * Counts the number of pieces of type Cell
 */

countPieces(Board, Cell, Num) :-
    findall(_, getCell(Board, _, _, Cell), Cells),
    length(Cells, Num).

/**
 * evaluateBoard(+State, -Value)
 *
 * The board is better with a smaller Value (0 is a win)
 */

evaluateBoard((Board, r), Value) :-

    countPieces(Board, rJ, Num1),
    countPieces(Board, rS, Num2),
    PlayerValue is Num1+Num2*2, % weight of the player having a skipper is higher for the player

    countPieces(Board, bJ, Num3),
    countPieces(Board, bS, Num4),
    OpponentValue is Num3*2+Num4, % weight of the opponent having a jumper is higher for the player

    overallValue(PlayerValue, OpponentValue, Value).

evaluateBoard((Board, b), Value) :-

    countPieces(Board, bJ, Num1),
    countPieces(Board, bS, Num2),
    PlayerValue is Num1+Num2*2, % weight of the player having a skipper is higher for the player

    countPieces(Board, rJ, Num3),
    countPieces(Board, rS, Num4),
    OpponentValue is Num3*2+Num4, % weight of the opponent having a jumper is higher for the player

    overallValue(PlayerValue, OpponentValue, Value).

/**
 * overallValue(+PlayerValue, +OpponentValue, -Value)
 *
 * Calculates a value based on the player and opponent's values
*/
overallValue(_, 0, 0). % win

overallValue(PlayerValue, OpponentValue, Value) :-
    PlayerValue > OpponentValue,
    Value is PlayerValue.

overallValue(PlayerValue, OpponentValue, Value) :-
    PlayerValue =< OpponentValue,
    Value is OpponentValue.