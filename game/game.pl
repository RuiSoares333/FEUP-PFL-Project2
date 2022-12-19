:-use_module(library(random)).

/**
 * gameInit(+BoardSize, +GameType)
 *
 * Initiates the game
*/
gameInit(BoardSizeN, BoardSizeM, P1-P2) :-
    initialState(BoardSizeN, BoardSizeM, GameState),
    displayGame(GameState),
    random_select(FirstPlayer, [P1, P2], _Rest),
    gameLoop(GameState, FirstPlayer, P1-P2).

/**
 * gameLoop(+GameState, +PlayerType, +GameType)
 *
 * Main game cycle
*/
gameLoop(GameState, PlayerType, GameType) :-
    gameOver(GameState, Winner), !,
    congratulateWinner(Winner).

gameLoop(GameState, PlayerType, GameType) :-
    chooseMove(GameState, PlayerType, Move),
    move(GameState, Move, NewGameState),
    nextPlayer(PlayerType, NewPlayerType, GameType),
    displayGame(NewGameState),
    gameLoop(NewGameState, NewPlayerType, GameType), !.

gameLoop(GameState, p, GameType) :-
    printInvalidMove,
    gameLoop(GameState, p, GameType), !. % Ask for another move

/**
 * chooseMove(+GameState, +PlayerType, -Move)
 *
 * Chooses the move according to the game state and player type
*/
chooseMove((Board, Player), p, Move) :-
    playerString(Player, PString),
    askTypeOfMove(PString, Num),
    boardDimensions(Board, LineNumber, ColumnNumber),
    chooseTypeOfMove(Num, LineNumber, ColumnNumber, Move).

chooseMove(GameState, Level, Move) :-
    validMoves(GameState, Moves),
    chooseMove(Level, GameState, Moves, Move).

chooseMove(e, (_Board, Player), Moves, Move) :-
    random_select(Move, Moves, _Rest),
    displayBotMove(Move, Player).

chooseMove(h, (Board, Player), Moves, Move) :-
    setof(Value-Mv, (NewBoard, Opponent)^( member(Mv, Moves),
        move((Board, Player), Mv, (NewBoard, Opponent)),
        switchColor(Opponent, Player),
        evaluateBoard((NewBoard, Player), Value) ), [_V-Move|_]),
    displayBotMove(Move, Player).

nextPlayer(p, p, p-p).
nextPlayer(p, Level, p-Level).
nextPlayer(Level, p, p-Level).
nextPlayer(Level1, Level2, Level1-Level2).
nextPlayer(Level2, Level1, Level1-Level2).

/**
 * chooseTypeOfMove(+MoveType, +LineNumber, +ColumnNumber, -Move)
 *
 * Chooses the move according to the move type
*/
chooseTypeOfMove(0, LineNumber, ColumnNumber, Move) :- 
    askForBoardPosition(LineNumber, ColumnNumber, Move).

chooseTypeOfMove(1, LineNumber, ColumnNumber, (X, Y)-(X1, Y1)) :-
    askForBoardPosition(LineNumber, ColumnNumber, (X, Y)),
    askForDirection((XOffset, YOffset)),
    X1 is X + XOffset,
    Y1 is Y + YOffset.