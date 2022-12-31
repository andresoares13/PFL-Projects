%predicate that will receive a user input (within itself, not the argument Number) and will convert the input into a number 
%input_number(-Number)
input_number(Number) :-
    flush_output,
    input_number(0, Number).

input_number(Acc, Number) :- %checks multiple cases, note that if the input is a period it will go back to menu, which is an option the user has here and when choosing the move
    get_code(Code),
    (   Code >= 48, Code =< 57
    ->  Acc1 is Acc * 10 + (Code - 48),
        input_number(Acc1, Number)
    ;   Code = 46
    ->  get_code(_),
        menu
    ;   Code >= 65, Code =< 90
    ->  flush_output,
        input_number(0, Number)
    ;   Code >= 97, Code =< 122
    ->  flush_output,
        input_number(0, Number)
    ;   Code > 122
    ->  flush_output,
        input_number(0, Number)
    ;   Acc = Number
    ).



%checks if a given atom is a number 
%is_number(+Atom)
is_number(Atom) :-
    atom_codes(Atom, Codes),
    maplist(is_digit, Codes).

%used inside a maplist, to see if all the ASCII codes are between 48 and 57 (0 to 9)
%is_digit(Code)
is_digit(Code) :-
    Code >= 48,
    Code =< 57.

%converts an Atom to a Number (Our version does not have the atom_number predicate so we made our own)
%atom_to_number(+Atom,-Number)
atom_to_number(Atom, Number) :-
    name(Atom, Codes),
    number_codes(Number, Codes).

%reverse of the previous predicate
%convertNumberToAtom(+Number,-Atom)
convertNumberToAtom(Number, Atom) :-
    number_codes(Number, Codes),
    atom_codes(Atom, Codes).



% Controls the input of the desired piece. Has multiple cases for full input control, it returns a NewPiece if valid, this piece will be 'D1' and not just '1' for example, the Move here is the Game Move
%checkInputPiece(+Board,+Piece,+Player,-NewPiece,+Move,+Length) %Piece here is an atom
checkInputPiece(_,'.',_,_,_,_) :- menu. %allows to go back to menu

checkInputPiece(Board,Piece,Player,NewPiece,_,Length) :- %case where it is valid 
    is_number(Piece),
    Piece @> '0',
    atom_to_number(Piece,NumberPiece),
    NumberPiece =< Length, %the piece is between the possible pieces given length
    getPieceByAtom(Board,_,_,Piece,Player,NewPiece). % test to see if the piece is still alive
   

checkInputPiece(Board,Piece,Player,_,Move,Length) :- %the piece is smaller than atom '1'
    is_number(Piece),
    Piece @< '1',
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.

checkInputPiece(Board,Piece,Player,_,Move,Length) :- %the piece is bigger than the Length of the board
    is_number(Piece),
    atom_to_number(Piece,NumberPiece),
    NumberPiece > Length,
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.   

checkInputPiece(Board,Piece,Player,_,Move,Length) :- %the atom of the piece is not a number
    \+ is_number(Piece),
    nl,nl,write('Please write a number, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.     

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    is_number(Piece),
    atom_to_number(Piece,NumberPiece),
    NumberPiece =< Length,
    Piece @> '0',
    \+ getPiece(Board,_,_,Piece,Player,_), %the piece should exist but it was captured 
    nl,nl,write('That piece was captured, Please choose an existing piece'),nl,nl,
    asserta(state(Board-Player-Move-Length)),
    fail.

%we ask the user to input letters as moves to make it simpler so this predicate converts the letters into the delta of X and Y of those moves
%translateMove(+Move,-Xdif,-Ydif)
translateMove('q',-1,1).
translateMove('w',0,1).
translateMove('e',1,1).
translateMove('a',-1,-1).
translateMove('s',0,-1).
translateMove('d',1,-1).


%the moves a player and its piece can make are really limited so we have this predicate that easily covers it all and tells us whether a move by a piece of a player is valid or not
%validPlayerPieceMove(+Player,+Letter,+Move)
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


%pieces cant capture vertically so we need to check if the capture being made is vertical or not
%verticalCapture(+Board,+X,+Y,+Player,+Move)
verticalCapture(Board,X,Y,Player,'w') :- %the only moves that produce vertical captures are w and s
    getPieceByPos(Board,X,Y,Piece),
    getFirstLetter(Piece,Letter),
    changePlayer(Player,OtherPlayer),
    validPiece(OtherPlayer,Letter).

verticalCapture(Board,X,Y,Player,'s') :-
    getPieceByPos(Board,X,Y,Piece),
    getFirstLetter(Piece,Letter),
    changePlayer(Player,OtherPlayer),
    validPiece(OtherPlayer,Letter).


%we cant move if we already have a piece there so this predicate checks if that is the case
%ownPiece(+Board,+X,+Y,+Player)
ownPiece(Board,X,Y,Player) :-
    getPieceByPos(Board,X,Y,Piece),
    getFirstLetter(Piece,Letter),
    validPiece(Player,Letter). %if there is a valid piece given a player in the location given than the piece is their own



% Controls the input of the desired move, checks many cases to let the user know exactly the problem, it also returns the X and Y of the final position where the move is being made to

%checkInputMove(+Board,+Piece,+Player,+Move,-Xsum,-Ysum,+Length,+GameMove)
checkInputMove(_,_,_,'.',_,_,_,_) :- %allows user to go back to menu
    menu.

checkInputMove(Board,Piece,Player,Move,Xsum,Ysum,Length,_) :- %valid move case, as it is in the list given by the predicate valid_moves
    valid_moves(Board-Length-Piece,Player,MoveList),
    member(Move,MoveList),
    translateMove(Move,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,_),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif.


checkInputMove(Board,_,Player,Move,_,_,Length,GameMove) :- %the move cannot be translated so the input was just trash
    \+translateMove(Move,_,_),
    nl,nl,write('That move does not exist'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.
    

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :- %the move cannot be executed by that piece
    translateMove(Move,_,_),
    atom_chars(Piece,[Letter|_]),
    \+validPlayerPieceMove(Player,Letter,Move),
    nl,nl,write('That move is not valid'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :- %the next 4 cases are the move going out of the 4 different bounds of the board
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


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :- %case where the user tried to move to a cell where they already have a piece
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
    Ysum < Height + 1,
    ownPiece(Board,Xsum,Ysum,Player),
    nl,nl,write('You have a piece there, choose another move'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.   


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :- %case where the user tried to capture vertically
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
    Ysum < Height + 1,
    \+ ownPiece(Board,Xsum,Ysum,Player),
    verticalCapture(Board,Xsum,Ysum,Player,Move),
    nl,nl,write('You cannot capture vertically, choose another move'),nl,nl,
    asserta(state(Board-Player-GameMove-Length)),
    fail.   


% Controls the input of the boards dimensions
%newDimensions(+Length,+Height)
newDimensions(Length,Height) :- %the dimensions are valid
    Length @>2,
    Height @>2,
    Length @<23,
    Height @<23.

newDimensions(Length,_) :- %the next 4 show invalid dimensions so they are discarded and thhe user is asked to input again
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


%controls the input of the computer level, there are only two (1 and 2) so it is simpler
%newComputerLevel(+Level)
newComputerLevel('1') :-
    write('                      The Computer Level has been set to: '),
    write('Easy'),nl,nl,
    retract(level(_)),
    asserta(level(1)),
    menu.

newComputerLevel('2') :-
    write('                      The Computer Level has been set to: '),
    write('Hard'),nl,nl,
    retract(level(_)),
    asserta(level(2)),
    menu.

newComputerLevel(_) :-
    write('                      Invalid Level, Please Try Again! '),
    settingsController('2').