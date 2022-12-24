input_number(Number) :-
    flush_output,
    input_number(0, Number).

input_number(Acc, Number) :-
    get_code(Code),
    (   Code >= 48, Code =< 57
    ->  Acc1 is Acc * 10 + (Code - 48),
        input_number(Acc1, Number)
    ;   Code >= 65, Code =< 90
    ->  flush_output,
        input_number(0, Number)
    ;   Code >= 97, Code =< 122
    ->  flush_output,
        input_number(0, Number)
    ;   Acc = Number
    ).

%checks if a given atom is a number or just letters
is_number(Atom) :-
    atom_codes(Atom, Codes),
    maplist(is_digit, Codes).

is_digit(Code) :-
    Code >= 48,
    Code =< 57.

atom_to_number(Atom, Number) :-
    name(Atom, Codes),
    number_codes(Number, Codes).


convertNumberToAtom(Number, Atom) :-
    number_codes(Number, Codes),
    atom_codes(Atom, Codes).



% Controls the input of the desired piece.

checkInputPiece(_,'.',_,_,_,_) :- menu.

checkInputPiece(Board,Piece,Player,NewPiece,_,Length) :-
    is_number(Piece),
    Piece @> '0',
    atom_to_number(Piece,NumberPiece),
    NumberPiece =< Length,
    getPiece(Board,_,_,Piece,Player,NewPiece). % test to see if the piece is still alive
   

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    is_number(Piece),
    Piece @< '1',
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    is_number(Piece),
    atom_to_number(Piece,NumberPiece),
    NumberPiece > Length,
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.   

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    \+ is_number(Piece),
    nl,nl,write('Please write a number, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.     

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    is_number(Piece),
    atom_to_number(Piece,NumberPiece),
    NumberPiece =< Length,
    Piece @> '0',
    \+ getPiece(Board,_,_,Piece,Player,_),
    nl,nl,write('That piece was captured, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.


translateMove('q',-1,1).
translateMove('w',0,1).
translateMove('e',1,1).
translateMove('a',-1,-1).
translateMove('s',0,-1).
translateMove('d',1,-1).

validPlayerPieceMove(1,'S','a').
validPlayerPieceMove(1,'S','s').
validPlayerPieceMove(1,'S','d').
validPlayerPieceMove(1,'D','q').
validPlayerPieceMove(1,'D','w').
validPlayerPieceMove(1,'D','e').
validPlayerPieceMove(2,'s','q').
validPlayerPieceMove(2,'s','w').
validPlayerPieceMove(2,'s','e').
validPlayerPieceMove(2,'d','a').
validPlayerPieceMove(2,'d','s').
validPlayerPieceMove(2,'d','d').


% Controls the input of the desired move


checkInputMove(Board,Piece,Player,Move,Xsum,Ysum,Length,_) :-
    translateMove(Move,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,_),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Xsum is Xi + Xdif,
    Xsum > 0,
    Xsum < Length + 1,
    Ysum is Yi + Ydif,
    Ysum > 0,
    length(Board,Height),
    Ysum < Height + 1.

checkInputMove(Board,_,Player,Move,_,_,Length,GameMove) :-
    \+translateMove(Move,_,_),
    nl,nl,write('That move does not exist'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.
    

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,_,_),
    atom_chars(Piece,[Letter|_]),
    \+validPlayerPieceMove(Player,Letter,Move),
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,Xdif,_),
    getPiece(Board,Xi,_,Piece,Player,_),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Xsum is Xi + Xdif,
    Xsum =< 0,
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,Xdif,_),
    getPiece(Board,Xi,_,Piece,Player,_),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Xsum is Xi + Xdif,
    Xsum > Length,
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,_,Ydif),
    getPiece(Board,_,Yi,Piece,Player,_),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Ysum is Yi + Ydif,
    Ysum =< 0,
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,_,Ydif),
    getPiece(Board,_,Yi,Piece,Player,_),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Ysum is Yi + Ydif,
    length(Board,Height),
    Ysum > Height,
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.   



% Controls the input of the boards dimensions

newDimensions(Length,Height) :-
    Length @>2,
    Height @>2,
    Length @<23,
    Height @<23.

newDimensions(Length,_) :-
    Length @<3,
    nl,nl,write('             Invalid Dimensions, Please Try Again!  '),nl,nl,
    flush_output,
    settingsController('1').

newDimensions(Length,_) :-
    Length @>22,
    nl,nl,write('             Invalid Dimensions, Please Try Again!  '),nl,nl,
    flush_output,
    settingsController('1').

newDimensions(_,Height) :-
    Height @<3,
    nl,nl,write('             Invalid Dimensions, Please Try Again!  '),nl,nl,
    flush_output,
    settingsController('1').

newDimensions(_,Height) :-
    Height @>22,
    nl,nl,write('             Invalid Dimensions, Please Try Again!  '),nl,nl,
    flush_output,
    settingsController('1').