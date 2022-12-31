%predicate that displays the menu and its options
%menu()
menu:-
    nl,nl,write('                         Ugly Duck'),write('            __'),nl,
    write('                                            <(o )___'),nl,
    write('                         Main Menu'),write('           ( ._> /'),nl,
    write('                                              `---'),write('\x00b4\'),nl,
    nl,nl,write('                    Pick an option (1-4)'),nl,nl,
    write('                      1. Play Game'),nl,nl,
    write('                      2. Change Settings'),nl,nl,
    write('                      3. Instructions'),nl,nl,
    write('                      4. Exit'),nl,nl,
    get_char(Option),
    get_char(_),
    menuController(Option).

%predicate that will display after an option from the menu was chosen and will give new options
%menuController(+Option)
menuController('1') :- %this case is the play for example
    nl,nl,nl,nl,
    write('                      1. Player VS Player'),nl,nl,
    write('                      2. Player VS Computer'),nl,nl,
    write('                      3. Computer VS Computer'),nl,nl,
    write('                      4. Go Back to Main Menu'),nl,nl,
    get_char(Option),
    get_char(_),
    playController(Option).


menuController('2') :- %settings
    nl,nl,nl,nl,
    write('                      1. Change Board Dimensions'),nl,nl,
    write('                      2. Change Computer Level'),nl,nl,
    write('                      3. Go Back to Main Menu'),nl,nl,
    get_char(Option),
    get_char(_),
    settingsController(Option).


menuController('3') :- %intructions
    nl,nl,write('                         Ugly Duck'),write('            __'),nl,
    write('                                            <(o )___'),nl,
    write('                         Instructions'),write('        ( ._> /'),nl,
    write('                                              `---`'),nl,nl,
    write('     DUCK: A duck moves one cell orthogonal and diagonal forward'),nl,
    write('        - Ducks capture diagonal forward. Captures are not mandatory.'),nl,
    write('        - A duck reaching last row is promoted to a swan.'),nl,nl,
    write('     SWAN: A swan moves one cell orthogonal and diagonal backward'),nl,
    write('        - Swans capture diagonal backward. Captures are not mandatory.'),nl,nl,
    write('     GOAL: Wins the player who first moves a swan into his first row.'),nl,nl,nl,
    write('     If at any point during the game you wish to go back to the main menu,'),nl,
    write('       just enter . (period) when asked for the piece or for the move.'),nl,nl,nl,
    write('     The pieces of player 1 have their letters in uppercase while the pieces'),nl,
    write('       of player 2 are inin lowercase, they can be either a D/d or S/s.'),nl,nl,nl,
    write('     You can change both the board dimensions and computer level in the settings.'),nl,nl,nl,
    write('                     0. Go Back to Main Menu'),nl,nl,
    get_char(Option),
    get_char(_),
    instructionsController(Option).

menuController('4') :- halt. %exit

menuController(_) :- %invalid input
    nl,write('             Invalid Input, Please Try Again!  '),nl,
    get_char(Option),
    get_char(_),
    menuController(Option).


%given a tuple size (length-Height) returns a board with that size, already containing the players pieces
%initial_state(+Size, -GameState)
initial_state(Length-Height, Board) :-  
    createBoard(Length,Height,Board).

%predicate that given a play option starts the mode selected
%playController(+Option)
playController('1') :-  %Player VS Player
    retract(boardSettings(Length-Height)),
    initial_state(Length-Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoop.

playController('2') :- %Player VS Computer
    retract(boardSettings(Length-Height)),
    initial_state(Length-Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoopHumanComputer.


playController('3') :- %Computer VS Computer
    retract(boardSettings(Length-Height)),
    initial_state(Length-Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoopComputerComputer.


playController('4') :- menu. %back to menu


playController(_):- %invalid input
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    playController(Option).


%controls the instructions
%instructionsController(+Option)
instructionsController('0'):- %back to menu
    prompt(_, ''),
    menu.
instructionsController(_):- %invalid input
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    instructionsController(Option).




%controls the settings 
settingsController('1'):- %allows to change the size of the board
    nl,nl,nl,nl,write('Min: 3  | Max: 22'),nl,nl,
    write('Board Length: '), input_number(Length),
    nl,nl,
    write('Board Height: '), input_number(Height),
    nl,nl,nl,nl,
    newDimensions(Length,Height),
    write('                      The Boards Dimensions have been set to '),
    write(Length),write(' X '),write(Height),nl,nl,
    retract(boardSettings(_-_)),
    asserta(boardSettings(Length-Height)),
    menu.




settingsController('2'):- %allows to change the level of the AI
    nl,nl,nl,nl,
    write('                      1. Easy'),nl,nl,
    write('                      2. Hard'),nl,nl,
    get_char(Option),
    get_char(_),
    newComputerLevel(Option).
settingsController('3'):- %back to menu
    prompt(_, ''),
    menu.    
settingsController(_):- %invalid input
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    settingsController(Option).    