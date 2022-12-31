:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(draw).
:- consult(input).
:- consult(menu).
:- consult(computer).
:- consult(logic).

:- dynamic boardSettings/1. %stores the length and height of the board, we need to store this since these values can be changed
:- dynamic state/1. %GameState, stores relevant game information such as board, player (the one that plays next), current move and board length
:- dynamic level/1. %stores the level of the computer




%main function and game loops


%play() this predicate starts the game itself and displays the menu
play:- 
    prompt(_, ''),
    asserta(boardSettings(5-5)),
    asserta(level(1)),
    menu.



%gameLoop() this is the main game loop of player vs player where the current player to move is asked to select a piece and a move, it then moves the piece, calculates states like end of the game or promote duck to swan and displays board
%this is a failure driven loop as this allows us to not use recursion and save memory that way
gameLoop :- 
    repeat,
    retract(state(Board-Player-Move-Length)),
    writePossiblePieces(Board,Player),
    write('Choose your piece: '), 
    input_number(NumberPiece),
    convertNumberToAtom(NumberPiece,Piece),
    checkInputPiece(Board,Piece,Player,NewPiece,Move,Length),
    writePossibleMoves(Board,Player,Length,NewPiece),
    write('Choose your move: '), 
    get_char(PlayerMove),
    skip_line,
    move(Board-NewPiece-Player-Length-Move,PlayerMove,Finalboard),
    checkStateEnd(Finalboard,Player),
    changePlayer(Player,NewPlayer),  %if the game is not over, we change the player and increment the move count
    incrementMove(Move,NewMove),nl,nl,
    display_game(Finalboard-NewMove-NewPlayer-Length),
    asserta(state(Finalboard-NewPlayer-NewMove-Length)),
    fail.


%gameLoopHumanComputer() %this is the loop of the player vs computer mode, similar to the player vs player but now it is just 1 player, after the player moves the computer moves, and its move depends on the level of difficulty (the default is easy)
gameLoopHumanComputer :-
    repeat,
    retract(state(Board-Player-Move-Length)),
    writePossiblePieces(Board,Player),
    write('Choose your piece: '), 
    input_number(NumberPiece),
    convertNumberToAtom(NumberPiece,Piece),
    checkInputPiece(Board,Piece,Player,NewPiece,Move,Length),
    writePossibleMoves(Board,Player,Length,NewPiece),
    write('Choose your move: '), 
    get_char(PlayerMove),write(PlayerMove),
    skip_line,
    move(Board-NewPiece-Player-Length-Move,PlayerMove,Finalboard),
    checkStateEnd(Finalboard,Player),
    incrementMove(Move,NewMove),nl,nl,
    retract(level(Level)),
    write(Level),
    computerMove(Finalboard-Length,2,Level,ComputerBoard),
    checkStateEnd(ComputerBoard,2),
    incrementMove(NewMove,ComputerMove),nl,nl,
    display_game(ComputerBoard-ComputerMove-Player-Length),
    asserta(state(ComputerBoard-Player-ComputerMove-Length)),
    asserta(level(Level)),
    fail.

%gameLoopComputerComputer() %loop of the mode computer vs computer, after each move, the game freezes for 2 seconds, this allows the user to see the moves since they are automatic and therefore quite fast, the move the computers make also depend on their levels
gameLoopComputerComputer :-
    repeat,
    retract(state(Board-Player-Move-Length)),
    retract(level(Level)),
    asserta(level(Level)),
    sleep(2),
    computerMove(Board-Length,Player,Level,ComputerBoard),
    checkStateEnd(ComputerBoard,Player),
    incrementMove(Move,ComputerMove),nl,nl,
    changePlayer(Player,NewPlayer),
    display_game(ComputerBoard-ComputerMove-NewPlayer-Length),
    asserta(state(ComputerBoard-NewPlayer-ComputerMove-Length)),
    fail.
   



