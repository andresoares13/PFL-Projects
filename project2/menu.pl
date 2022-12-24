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


menuController('1') :-
    nl,nl,nl,nl,
    write('                      1. Player VS Player'),nl,nl,
    write('                      2. Player VS Computer'),nl,nl,
    write('                      3. Computer VS Computer'),nl,nl,
    write('                      4. Go Back to Main Menu'),nl,nl,
    get_char(Option),
    get_char(_),
    playController(Option).


menuController('2') :-
    nl,nl,nl,nl,
    write('                      1. Change Board Dimensions'),nl,nl,
    write('                      2. Change Computer Level'),nl,nl,
    write('                      3. Go Back to Main Menu'),nl,nl,
    get_char(Option),
    get_char(_),
    settingsController(Option).


menuController('3') :-
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
    write('                     0. Go Back to Main Menu'),nl,nl,
    write('                     1. Exit'),nl,nl,
    get_char(Option),
    get_char(_),
    instructionsController(Option).

menuController('4') :- halt.

menuController(_) :-
    nl,write('             Invalid Input, Please Try Again!  '),nl,
    get_char(Option),
    get_char(_),
    menuController(Option).



playController('1') :-
    retract(boardSettings(Length-Height)),
    createBoard(Length,Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoop.

playController('2') :-
    retract(boardSettings(Length-Height)),
    createBoard(Length,Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoopHumanComputer.


playController('3') :- 
    retract(boardSettings(Length-Height)),
    createBoard(Length,Height,Board),
    display_game(Board-1-1-Length),
    asserta(state(Board-1-1-Length)),
    asserta(boardSettings(Length-Height)),
    gameLoopComputerComputer.


playController('4') :- menu.


playController(_):-
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    playController(Option).


instructionsController('0'):-
    prompt(_, ''),
    menu.
instructionsController('1'):-
    halt.
instructionsController(_):-
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    instructionsController(Option).





settingsController('1'):-
    nl,nl,nl,nl,
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




settingsController('2'):-
    nl,nl,nl,nl,
    menu.
settingsController('3'):-
    prompt(_, ''),
    menu.    
settingsController(_):-
    nl,nl,write('             Invalid Input, Please Try Again!  '),nl,nl,
    get_char(Option),
    get_char(_),
    settingsController(Option).    