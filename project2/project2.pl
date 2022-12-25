:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(draw).
:- consult(input).
:- consult(menu).
:- consult(computer).

:- dynamic boardSettings/1.
:- dynamic state/1.
:- dynamic level/1.


createBoardSpaces(Board,_,0,Board).
createBoardSpaces(Board,Length,Height,NewBoard) :-
    Height > 0,
    H is Height - 1,
    createRowSpaces([],Length,NewRow),
    append(Board,[NewRow],Board2),
    createBoardSpaces(Board2,Length,H,NewBoard).


createRowSpaces(Row,0,Row).
createRowSpaces(Row,Length,NewRow) :-
    Length > 0,
    L is Length - 1,
    append(Row,['  '], Row2),
    createRowSpaces(Row2,L,NewRow).


createPlayer1(Row,0,_,Row).
createPlayer1(Row,Length,Counter,NewRow) :-
    Length > 0,
    number_codes(Counter,Code),
    atom_codes(A,Code),
    atom_concat('D',A,Duck),
    C is Counter + 1,
    L is Length - 1,
    append(Row,[Duck],Row2),
    createPlayer1(Row2,L,C,NewRow).

createPlayer2(Row,0,_,Row).
createPlayer2(Row,Length,Counter,NewRow) :-
    Length > 0,
    number_codes(Counter,Code),
    atom_codes(A,Code),
    atom_concat('d',A,Duck),
    C is Counter + 1,
    L is Length - 1,
    append(Row,[Duck],Row2),
    createPlayer2(Row2,L,C,NewRow).


createBoard(Length,Height,Board) :-
    createPlayer1([],Length,1,NewRow),
    SpacesHeight is Height - 2,
    createBoardSpaces([NewRow],Length,SpacesHeight,NewBoard),
    createPlayer2([],Length,1,NewRow2),
    append(NewBoard,[NewRow2],Board).




replace_at_index(X,Y,List,Index,NewList) :-
    replace_at_index(X,Y,List,Index,1,NewList).

replace_at_index(_,_,[],_,_,[]).
replace_at_index(X,Y,[X|Xs],Index,CurrentIndex,[Y|Xs]) :-
    Index =:= CurrentIndex.                                  %ask teacher about the validity of this operator
replace_at_index(X,Y,[Z|Xs],Index,CurrentIndex,[Z|Zs]) :-
    CurrentIndex < Index,
    NewIndex is CurrentIndex + 1,
    replace_at_index(X,Y,Xs,Index,NewIndex,Zs).



% getPiece(-Board,-X,-Y,+Piece)
getPiece(Board,X,Y,Piece,Player,ActualPiece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,ActualPiece),
    atom_concat(_,Piece,ActualPiece),
    getFirstLetter(ActualPiece,Letter),
    validPiece(Player,Letter).


getPieceByPos(Board,X,Y,Piece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,Piece).

getFirstLetter(Atom, FirstLetter) :-
    atom_chars(Atom, AtomChars),
    nth1(1, AtomChars, FirstLetter).


validPiece(1,'D').
validPiece(1,'S').
validPiece(2,'d').
validPiece(2,'s').


setPiece(Board,X,Y,Piece,Newboard) :-
    nth1(Y,Board,Line),
    nth1(X,Line,X1),
    replace_at_index(X1,Piece,Line,X,NewLine),
    replace_at_index(Line,NewLine,Board,Y,Newboard).

clearPiece(Board,X,Y,Newboard) :-
    nth1(Y,Board,Line),
    nth1(X,Line,X1),
    replace_at_index(X1,'  ',Line,X,NewLine),
    replace_at_index(Line,NewLine,Board,Y,Newboard).

movePiece(Board,Piece,X2,Y2,Player,SwanBoard) :-
    getPiece(Board,X,Y,Piece,Player,_),
    setPiece(Board,X2,Y2,Piece,Newboard),
    clearPiece(Newboard,X,Y,Finalboard),
    checkStateSwan(Finalboard,Player,SwanBoard).


evolvePiece(Piece,Atom,1) :-
    atom_chars(Piece,Chars),
    select('D', Chars, NewChars),
    maplist(char_code, ['S' | NewChars], Codes),
    name(Atom, Codes).

evolvePiece(Piece,Atom,2) :-
    atom_chars(Piece,Chars),
    select('d', Chars, NewChars),
    maplist(char_code, ['s' | NewChars], Codes),
    name(Atom, Codes).    


convertPos(X,Y,X2,Y2) :-
    number_codes(X2,[X]),
    number_codes(Y2,[Y]).
     
    
changePlayer(1,2).
changePlayer(2,1).

getPlayerSwan(1,Board,Line,'D') :-
    length(Board,Line).

getPlayerSwan(2,_,1,'d').

checkStateSwan(Board,Player,NewBoard) :-
    getPlayerSwan(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter),
    nth1(X,Line,M),
    evolvePiece(M,Swan,Player),
    setPiece(Board,X,LineNr,Swan,NewBoard).

checkStateSwan(Board,Player,Board) :-
    getPlayerSwan(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    \+ is_member(Letter,Line).

is_member(Element, [H|_]) :-
    sub_atom(H, 0, 1, _, FirstLetter),
    Element == FirstLetter.

is_member(Element, [_|T]) :-
    is_member(Element, T).


playerEnd(1,_,1,'S'). 

playerEnd(2,Board,Line,'s') :-
    length(Board,Line).


%see if the player still has any peaces alive, if all of the pieces are gone, the player without pieces loses
hasPieces(Board,Player) :-
    findall(Piece, (
        getPiece(Board,_,_,Piece,Player,_),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter)
    ), PieceList),
    length(PieceList,Len),
    Len > 0.
    


checkStateEnd(Board,Player) :-
    game_over(Board-Player,Player),
    winner(Player).

checkStateEnd(_,_).    


%game_over(+GameState, -Winner) %checks to see if the player won the game, to win the game, the swan either reached the players row or there are no more pieces
game_over(Board-Player, Player) :-   %if the given player won the game, then it unifies with Winner
    playerEnd(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter).

game_over(Board-Player, Player) :-
    changePlayer(Player,OtherPlayer),
    \+ hasPieces(Board,OtherPlayer).



incrementMove(Move,NewMove) :-
    NewMove is Move + 1.




%main function and game loops


play:- 
    prompt(_, ''),
    asserta(boardSettings(5-5)),
    asserta(level(1)),
    menu.


winner(Player) :-
    nl,nl,write('                                Player '),write(Player),write(' won'),nl,nl,
    menu.

gameLoop :- 
    repeat,
    retract(state(Board-Player-Move-Length)),
    write('Choose your piece: '), 
    input_number(NumberPiece),
    convertNumberToAtom(NumberPiece,Piece),
    checkInputPiece(Board,Piece,Player,NewPiece,Move,Length),
    write('Choose your move: '), 
    get_char(PlayerMove),
    skip_line,
    checkInputMove(Board,NewPiece,Player,PlayerMove,X2,Y2,Length,Move),
    movePiece(Board,NewPiece,X2,Y2,Player,Finalboard),
    checkStateEnd(Finalboard,Player),
    changePlayer(Player,NewPlayer),
    incrementMove(Move,NewMove),nl,nl,
    display_game(Finalboard-NewMove-NewPlayer-Length),
    asserta(state(Finalboard-NewPlayer-NewMove-Length)),
    fail.


gameLoopHumanComputer :-
    repeat,
    retract(state(Board-Player-Move-Length)),
    write('Choose your piece: '), 
    input_number(NumberPiece),
    convertNumberToAtom(NumberPiece,Piece),
    checkInputPiece(Board,Piece,Player,NewPiece,Move,Length),
    write('Choose your move: '), 
    get_char(PlayerMove),write(PlayerMove),
    skip_line,
    checkInputMove(Board,NewPiece,Player,PlayerMove,X2,Y2,Length,Move),
    movePiece(Board,NewPiece,X2,Y2,Player,Finalboard),
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
   


