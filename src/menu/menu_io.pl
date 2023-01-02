/*********************/
/****** INPUT *******/
/*******************/


/**
 * readUntilBetweenAndEven(+Min, +Max, -Value)
 *
 * Reads a number from input until the user inserts an odd one between two values
 */
readUntilBetweenAndEven(Min, Max, Value) :-
    format('Choose an odd number between ~d and ~d: ', [Min, Max]),
    readNumber(Value),
    betweenAndEven(Min, Max, Value), !.

readUntilBetweenAndEven(Min, Max, Value) :-
    format('Invalid option! Remember it has to be an even number between ~d and ~d ~n', [Min, Max]),
    readUntilBetweenAndEven(Min, Max, Value).



/*********************/
/****** OUTPUT ******/
/*******************/


/**
 * menuTitle(+Title)
 *
 * Displays the menu's title, formatted with 75 chars of width
 */
menuTitle(Title) :-
    format('~n~`-t ~p ~`-t~75|~n', [Title]).


/**
 * menuOptionsHeader(+Options, +Captions)
 *
 * Displays the menu's options header, formatted with 75 chars of width
 * The two headers serve as descriptions for the options/captions
 */
menuOptionsHeader(Options, Captions) :-
    format('|~t~p~t~37+~t~p~t~37+~t|~75|~n', [Options, Captions]).


/**
 * menuEmptyLine/0
 *
 * Displays an empty line of the menu, formatted with 75 chars of width
 */
menuEmptyLine :-
    format('|~t|~75|~n', []).


/**
 * menuOption(+Option, +Caption)
 *
 * Displays a menu's option, formatted with 75 chars of width
 * Takes an option and the corresponding caption
 */
menuOption(Option, Caption) :-
    format('|~t~p~t~37|~t~p~t~37+~t|~75|~n', [Option, Caption]).


/**
 * menuTitle(+Title)
 *
 * Displays regular text in the menu, formatted with 75 chars of width
 */
menuText(Text) :-
    format('|~t~p~t|~75|~n', [Text]).


/**
 * menuFill/0
 *
 * Displays the filled line of the menu, formatted with 75 chars of width
 */
menuFill :-
    format('~`-t~75|~n', []).


/**
 * displayInstructions/0
 *
 * Displays the game's instructions
 */
displayInstructions :-
    menuText('************************ Game Board ************************'),
    menuEmptyLine,
    menuText('This game is played on a NxM rectangular'),
    menuText('board with a number of red and black pieces'),
    menuText('facing each other on each board edge'),

    menuEmptyLine,

    menuText('************************ Gameplay ************************'),
    menuEmptyLine,
    menuText('Red/Black jumpers move 1 cells to the right/left each time.'),
	menuText('They can also jump over an enemy piece directly below or above them.'),
    menuText('If that jumped piece is a Jumper, it is demoted to a Slipper.'),
    menuText('But be careful! You can also jump off of the board!'),
    menuText('When a piece moves off the board, it is removed'),

    menuEmptyLine,

    menuText('************************ How to Win ************************'),
    menuEmptyLine,
    menuText('Wins the player that made the last move.'),
    menuText('(Wins the player that has the last remaining piece)').