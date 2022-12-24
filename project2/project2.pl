:- use_module(library(lists)).
:- consult(draw).
:- consult(input).
:- consult(menu).

:- dynamic boardSettings/1.
:- dynamic state/1.


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
    Index =:= CurrentIndex.
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
    write('Letter: '),write(Letter),
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


checkStateEnd(Board,Player) :-
    playerEnd(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter),
    winner(Player).

checkStateEnd(_,_).    

incrementMove(Move,NewMove) :-
    NewMove is Move + 1.
    




play:- 
    prompt(_, ''),
    asserta(boardSettings(5-5)),
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
    drawGame(Finalboard,NewMove,NewPlayer,Length),
    asserta(state(Finalboard-NewPlayer-NewMove-Length)),
    fail.
   


