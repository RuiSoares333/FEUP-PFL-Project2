/**
 * validateShiftStone(+GameState, +Pos, +NewPos)
 */
validateShiftStone((Board, Player), (X, Y), (X, NewY), (NewXPos, NewYPos), (NewBoard, Player)) :-
    getCell(Board, X, Y, OwnCell) ,
    jumperState(OwnCell, Player),
    getCell(Board, X, NewY, OtherCell) ,
    enemyState(OtherCell, Player) ,
    getCell(Board, X, NewY, e),
    Abs is abs(NewY - Y),
    Abs =:= 1,

    makeJump(Board, (X, Y), (X, NewY), (NewXPos, NewYPos), NewBoard), !.

validateShiftStone((Board, Player), (X, Y), (NewX, Y), (NewXPos, NewYPos), (Board, Player)) :-
    number(NewX),
    number(Y),
    \+ checkMoveInBound(Board, (NewX, Y)), 

    getCell(Board, X, Y, Cell),
    state(Cell, Player), % Can only shift an owned stone
    NewXPos is NewX,
    NewYPos is Y.

validateShiftStone((Board, Player), (X, Y), (NewX, Y), (NewXPos, NewYPos), (Board, Player)) :-
    getCell(Board, X, Y, Cell),
    state(Cell, Player), % Can only shift an owned stone

    getCell(Board, NewX, Y, e), % Check if cell is empty

    verifyStuff(Player, NewX, X),
    NewXPos is NewX,
    NewYPos is Y.

verifyStuff(b, NewX, X) :-
    Val is NewX - X,
    Val =:= -1.

verifyStuff(r, NewX, X) :-
    Val is NewX - X,
    Val =:= 1.


% validateShiftStone((Board, Player), (X, Y), (NewX, Y), (NewXPos, NewYPos), (NewBoard, Player)) :-
%     write('4 '),
%     getCell(Board, X, Y, Cell),
%     state(Cell, Player), % Can only shift an owned stone
%     makeJump(Board, (X, Y), (NewX, Y), (NewXPos, NewYPos), NewBoard), !.

makeJump(Board, (X,Y), (NewX, NewY), (NewXPos, NewYPos), NewBoard) :-
    XPos is X + (NewX - X) * 2 ,
    YPos is Y + (NewY - Y) * 2,
    checkMoveInBound(Board, (XPos, YPos)),
    getCell(Board, NewX, NewY, JumperCell), % get the cell on the new position coordinates
    jumper2Skipper(JumperCell, SkipperCell), % change other player's cell to skipper
    replaceCell(Board, NewX, NewY, SkipperCell, NewBoard),
    assignNewPosition((X, Y), (NewX, NewY), (NewXPos, NewYPos)).

makeJump(Board, (X,Y), (NewX, NewY), (NewXPos, NewYPos), NewBoard) :-
    getCell(Board, NewX, NewY, JumperCell), % get the cell on the new position coordinates
    jumper2Skipper(JumperCell, SkipperCell), % change other player's cell to skipper
    replaceCell(Board, NewX, NewY, SkipperCell, NewBoard).

assignNewPosition((X, Y), (NewX, NewY), (NewXPos, NewYPos)) :-
    NewXPos is X + (NewX - X) * 2 ,
    NewYPos is Y + (NewY - Y) * 2.


/**
 * shiftStone(+GameState, +Pos, +NewPos, -NewGameState)
 */

 shiftStone((Board, Player), (X, Y)-(X, NewY), (NewBoard, NextPlayer)) :-
    number(X),
    number(NewY),
    checkMoveInBound(Board, (NewX, NewY)),
    makeJump(Board, (X, Y), (X, NewY), (NewXPos, NewYPos), NewBoard)
    switchColor(Player, NextPlayer).

shiftStone((Board, Player), (X, Y)-(NewX, NewY), (NewBoard, NextPlayer)) :-
    number(NewX),
    number(NewY),
    checkMoveInBound(Board, (NewX, NewY)),
    getCell(Board, X, Y, Cell),
    replaceCell(Board, X, Y, e, Board1),
    replaceCell(Board1, NewX, NewY, Cell, NewBoard),
    switchColor(Player, NextPlayer).

shiftStone((Board, Player), (X, Y)-(NewX, NewY), (NewBoard, NextPlayer)) :-
    replaceCell(Board, X, Y, e, NewBoard),
    switchColor(Player, NextPlayer).

checkMoveInBound(Board, (X, Y)) :-
    boardDimensions(Board, LineNumber, ColumnNumber),
    X >= 0,
    X < ColumnNumber,
    Y >= 0,
    Y < LineNumber.

/**
 * move(+GameState, +Move, -NewGameState)
 */

% move(GameState, Move) :-
    

move(GameState, (X, Y)-(X1, Y1), NewGameState) :-
    validateShiftStone(GameState, (X, Y), (X1, Y1), (X2, Y2), MiddleGameState),
    shiftStone(MiddleGameState, (X, Y)-(X2, Y2), NewGameState).


validMoves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves),
    write(Moves),
    write('\n').

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