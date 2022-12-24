/**
 * validateShiftStone(+GameState, +Pos, +NewPos)
 */
validateShiftStone((Board, Player), (X, Y), (NewX, NewY), (NewXPos, NewYPos)) :-
    getCell(Board, X, Y, Cell),
    state(Cell, Player), % Can only shift an owned stone

    getCell(Board, NewX, NewY, e), %check if cell is empty
    NewXPos is NewX, NewYPos is NewY, !.


validateShiftStone((Board, Player), (X, Y), (NewX, NewY), (NewXPos, NewYPos)) :-
    getCell(Board, X, Y, Cell),
    state(Cell, Player), % Can only shift an owned stone

    checkForOtherPlayer((Board, Player), (NewX, NewY)), % check if the other cell is occupied by other player
    getCell(Board, NewX, NewY, JumperCell), % get the cell on the new position coordinates
    jumper2Skipper(JumperCell, SkipperCell), % change other player's cell to skipper
    replaceCell(Board, NewX, NewY, SkipperCell, Board1),
    assignNewPosition((X, Y), (NewX, NewY), (NewXPos, NewYPos)), !. 



checkForOtherPlayer((Board, r), (X, Y)) :-
    getCell(Board, X, Y, bJ) ; getCell(Board, X, Y, bS).    

checkForOtherPlayer((Board, b), (X, Y)) :-
    getCell(Board, X, Y, rJ) ; getCell(Board, X, Y, rS).


assignNewPosition((X, Y), (NewX, NewY), (NewXPos, NewYPos)) :-
    NewXPos is X + (NewX - X)*2,
    NewYPos is Y + (NewY - Y)*2.


/**
 * shiftStone(+GameState, +Pos, +NewPos, -NewGameState)
 */
shiftStone((Board, Player), (X, Y), (NewX, NewY), (NewBoard, NextPlayer)) :-
    getCell(Board, X, Y, Cell),
    replaceCell(Board, X, Y, e, Board1),
    replaceCell(Board1, NewX, NewY, Cell, NewBoard),
    switchColor(Player, NextPlayer).

/**
 * move(+GameState, +Move, -NewGameState)
 */
move(GameState, (X, Y)-(X1, Y1), NewGameState) :-
    validateShiftStone(GameState, (X, Y), (X1, Y1), (X2, Y2)),
    shiftStone(GameState, (X, Y), (X2, Y2), NewGameState).

/**
 * checkWin(+Vectors, +Player)
 *
 * Check if the player won, by analyzing the list of vectors
 */
checkWin([], _) :- true.


checkWin([Vector | T], r) :- 
    sublist(Vector, [rJ], _) ; sublist(Vector, [rS], _).

checkWin([Vector | T], b) :- 
    sublist(Vector, [bJ], _) ; sublist(Vector, [bS], _).

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


validMoves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).


/**
 * evaluateVector(+Vector, +Player, +WinNum, -Value)
 *
 * Evaluates a vector by counting how many pieces you need to place in order to win in that vector.
 * If the opponent is blocking, Value is WinNum + minimum number of pieces the opponent needs to take out
 */
evaluateVector(Vector, Player, WinNum, Value) :-
    evaluateVectorSimmetric(Vector, Player, WinNum, Max),
    Value is WinNum - Max.

/**
 * evaluateVectorSimmetric(+Vector, +Player, +WinNum, -Max)
 *
 * Simetric of the evaluateVector function, to ease calculations
 */
evaluateVectorSimmetric(Vector, Player, WinNum, Max) :-
    Min is 0 - WinNum,
    evaluateVectorSimmetric(Vector, Player, WinNum, Max, 0, Min).
evaluateVectorSimmetric(Vector, _, WinNum, Max, Before, Max) :-
    length(Vector, Len),
    Len is Before + WinNum - 1.

evaluateVectorSimmetric(Vector, Player, WinNum, Max, Before, CurMax) :-
    sublist(Vector, SubVector, Before, WinNum),
    countFreePieces(SubVector, Player, Num),
    max_member(NewMax, [CurMax, Num]),
    Before1 is Before + 1,
    evaluateVectorSimmetric(Vector, Player, WinNum, Max, Before1, NewMax).

/**
 * countFreePieces(+List, +Player, -Num)
 *
 * Counts the number of player owned pieces not interrupted by the opponent
 * If the opponent has pieces in the list, Num is the simmetric of the opponent's number of pieces
 */
countFreePieces(List, Player, Num) :-
    switchColor(Player, Opponent),
    member(Opponent, List),
    countOcurrences(List, Opponent, OpponentOcurrences),
    Num is 0 - OpponentOcurrences, !.

countFreePieces(List, Player, Num) :-
    countOcurrences(List, Player, Num), !.

/**
 * evaluateBoard(+State, -Value)
 *
 * The board is better with a smaller Value (0 is a win)
 */
evaluateBoard((Board, Player), Value) :-
    transpose(Board, Cols),
    % These 2 are separated because the win condition is different
    whiteDiagonals(Board, WhiteDiagsOptions),
    append(Board, Cols, LinearOptions),

    mapsublist(state, LinearOptions, Linear),
    mapsublist(state, WhiteDiagsOptions, WhiteDiags),

    % Player Side
    setof(Val, Vector^( member(Vector, Linear), evaluateVector(Vector, Player, 5, Val) ), [ValueLinear|_]),
    setof(Val, Vector^( member(Vector, WhiteDiags), evaluateVector(Vector, Player, 4, Val) ), [ValueDiag|_]),
    min_member(PlayerValue, [ValueDiag, ValueLinear]),

    % Opponent Side
    switchColor(Player, Opponent),
    setof(Val, Vector^( member(Vector, Linear), evaluateVector(Vector, Opponent, 5, Val) ), OppLinearList),
    setof(Val, Vector^( member(Vector, WhiteDiags), evaluateVector(Vector, Opponent, 4, Val) ), OppDiagList),
    append(OppLinearList, OppDiagList, OpponentList),
    min_member(OpponentValue, OpponentList),

    overallValue(PlayerValue, OpponentValue, Value).

/**
 * overallValue(+PlayerValue, +OpponentValue, -Value)
 *
 * Calculates a value based on the player and opponent's base values
*/
overallValue(0, _, 0). % win
overallValue(PlayerValue, OpponentValue, Value) :-
    PlayerValue >= OpponentValue, % bad position, block opponent instead
    Value is 20 - OpponentValue.

overallValue(PlayerValue, OpponentValue, Value) :-
    PlayerValue < OpponentValue, % good position, play for yourself
    Value is 5 + PlayerValue - OpponentValue. % It's still better to block opponent if possible