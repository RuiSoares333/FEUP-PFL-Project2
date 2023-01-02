/**
 * moveOption(+Option, +Caption)
 *
 * Displays a player's move option
*/
moveOption(Option, Caption) :-
    format('   | ~p | ~p ~n', [Option, Caption]).


/**
 * printPlayerTurn(+Player)
 *
 * Displays the player's turn message
*/
printPlayerTurn(Player) :-
    format('>> Your turn, ~p <<~n~n', [Player]).

/**
 * readUntilValidDir(-Dir, Player)
 *
 * Asks the user for a direction and reads it
*/
readUntilValidDir(Dir, r) :-
    write('Choose a direction [t (Jump Top), b (Jump Bottom), r (Jump/Skip right)]: '), 
    get_char(Dir),
    skip_line,
    isValidDirection(Dir, r), !.

readUntilValidDir(Dir, b) :-
    write('Choose a direction [t (Jump Top), b (Jump Bottom), l (Jump/Skip left)]: '), 
    get_char(Dir),
    skip_line,
    isValidDirection(Dir, b), !.


readUntilValidDir(Dir, Player) :-
    write('Invalid move! Please choose one of the available moves.'), nl,
    readUntilValidDir(Dir, Player).

/**
 * readUntilValidRow(+RowNumber, -Row)
 *
 * Asks the user for a row and reads it
*/
readUntilValidRow(RowNumber, Row) :-
    format('Choose a row [1-~d]: ', [RowNumber]),
    readNumber(Row1),
    Row is Row1-1,
    between(1, RowNumber, Row1), !.

readUntilValidRow(RowNumber, Row) :-
    format('Invalid row! Please choose one between 1 and ~d~n', [RowNumber]),
    readUntilValidRow(RowNumber, Row).


/**
 * readUntilValidCol(+ColumnNumber, -Col)
 *
 * Asks the user for a column and reads it
*/
readUntilValidCol(ColumnNumber, Col) :-
    LastLetterCode is ColumnNumber+65-1,
    char_code(LastLetter, LastLetterCode),
    format('Choose a column [A-~p]: ', [LastLetter]),
    get_char(Char),
    skip_line,
    char_code(Char, CharCode),
    Col is CharCode-65,
    isLetterAndBounded(Char, ColumnNumber), !.

readUntilValidCol(ColumnNumber, Col) :-
    LastLetterCode is ColumnNumber+65-1,
    char_code(LastLetter, LastLetterCode),
    format('Invalid column! Please choose one between A and ~p~n', [LastLetter]),
    readUntilValidCol(ColumnNumber, Col).

/**
 * askForBoardPosition(+LineNumber, +ColumnNumber, Pos)
 *
 * Asks the user for a board position and reads it
*/
askForBoardPosition(LineNumber, ColumnNumber, (X, Y), (Board, Player)) :-
    playerString(Player, PString),
    printPlayerTurn(PString),
    readUntilValidCol(ColumnNumber, X),
    readUntilValidRow(LineNumber, Y),
    getCell(Board, X, Y, Cell),
    state(Cell, Player).

askForBoardPosition(LineNumber, ColumnNumber, (X, Y), (Board, Player)) :- 
    false.

/**
 * askForDirection(-Direction)
 *
 * Asks the user for a directions reads it
*/
askForDirection((DirX, DirY), Player) :-
    readUntilValidDir(Dir, Player),
    directionToOffsets(Dir, DirX, DirY).

/**
 * printInvalidMove/0
 *
 * Informs the user of an invalid move
*/
printInvalidMove :-
    nl, write('Invalid move.'), nl,
    write('Remember the rules and please try again'), nl, nl.

/**
 * congratulateWinner(+Winner)
 *
 * Congratulates the winner
*/
congratulateWinner(Loser) :-
    switchColor(Loser, Winner),
    playerString(Winner, PString),
    write('Congratulations, '),
    write(PString),
    write('! You won the game!'), skip_line.

playerString(r, 'Red').
playerString(b, 'Black').