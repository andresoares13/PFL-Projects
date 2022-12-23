tabuleiro([['D1','D2','D3','D4','D5'],['  ','  ','  ','  ','  '],['  ','  ','  ','  ','  '],['  ','  ','  ','  ','  '],['d1','d2','d3','d4','d5']]).
:- use_module(library(lists)).
:- consult(draw).
% piece(Type,Player,Pos).
% type can be D or S, Player can be 1 or 2, Pos is [X,Y] where X and Y are between 1 and 5 



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
    createBoardSpaces([NewRow],Length,Height,NewBoard),
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
getPiece(Board,X,Y,Piece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,Piece).


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
    getPiece(Board,X,Y,Piece),
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

convertPiece(Piece,1,NewPiece) :-
    atom_concat('D',Piece,NewPiece).

convertPiece(Piece,1,NewPiece) :-
    atom_concat('S',Piece,NewPiece).    

convertPiece(Piece,2,NewPiece) :-
    atom_concat('d',Piece,NewPiece).

convertPiece(Piece,2,NewPiece) :-
    atom_concat('s',Piece,NewPiece).          
    
changePlayer(1,2).
changePlayer(2,1).

getPlayerSwan(1,5,'D').
getPlayerSwan(2,1,'d').

checkStateSwan(Board,Player,NewBoard) :-
    getPlayerSwan(Player,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter),
    nth1(X,Line,M),
    evolvePiece(M,Swan,Player),
    setPiece(Board,X,LineNr,Swan,NewBoard).

checkStateSwan(Board,_,Board).        



playerEnd(1,1,'S').
playerEnd(2,5,'s').


checkStateEnd(Board,Player) :-
    playerEnd(Player,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter),
    winner(Player).

checkStateEnd(_,_).    

incrementMove(Move,NewMove) :-
    NewMove is Move + 1.
    




checkInputPiece(_,'.',_,_,_,_) :- menu.

checkInputPiece(Board,Piece,Player,NewPiece,_,_) :-
    Piece @> '0',
    nth1(1,Board,Line),
    length(Line,Length),
    L1 is Length + 1, % we want to compare if the atom is smaller so we add 1 to length to be able to do '5' @< '6' if the length is 5 for example
    number_codes(L1,[L|_]),
    char_code(Char,L),
    Piece @< Char, % this confirms that the chosen piece is between the possible pieaces given the length of the board which can vary
    convertPiece(Piece,Player,NewPiece), 
    getPiece(Board,_,_,NewPiece). % test to see if the piece is still alive

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    Piece @< '1',
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    gameLoop(Board,Player,Move,Length).

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    nth1(1,Board,Line),
    length(Line,Length),
    number_codes(Length,[L|_]),
    char_code(Char,L),
    Piece @> Char,
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    gameLoop(Board,Player,Move,Length).    

checkInputPiece(Board,Piece,Player,_,Move,Length) :-
    convertPiece(Piece,Player,NewPiece), 
    \+ getPiece(Board,_,_,NewPiece),
    nl,nl,write('That piece was captured, Please choose an existing piece'),nl,nl,
    gameLoop(Board,Player,Move,Length).   


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



checkInputMove(Board,Piece,Player,Move,X2,Y2,Length,_) :-
    translateMove(Move,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece),
    atom_chars(Piece,[Letter|_]),
    validPlayerPieceMove(Player,Letter,Move),
    Xsum is Xi + Xdif,
    Xsum > 0,
    Xsum < Length + 1,
    Ysum is Yi + Ydif,
    Ysum > 0,
    length(Board,Height),
    Ysum < Height + 1,
    number_codes(Xsum,[XsumA|_]),
    number_codes(Ysum,[YsumA|_]),
    convertPos(XsumA,YsumA,X2,Y2).

checkInputMove(Board,_,Player,Move,_,_,Length,GameMove) :-
    \+translateMove(Move,_,_),
    nl,nl,write('That move does not exist'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    atom_chars(Piece,[Letter|_]),
    \+validPlayerPieceMove(Player,Letter,Move),
    nl,nl,write('That move is not valid'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,Xdif,_),
    getPiece(Board,Xi,_,Piece),
    Xsum is Xi + Xdif,
    Xsum =< 0,
    nl,nl,write('That move is not valid'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,Xdif,_),
    getPiece(Board,Xi,_,Piece),
    Xsum is Xi + Xdif,
    Xsum > Length,
    nl,nl,write('That move is not valid'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).


checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,_,Ydif),
    getPiece(Board,_,Yi,Piece),
    Ysum is Yi + Ydif,
    Ysum =< 0,
    nl,nl,write('That move is not valid'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).

checkInputMove(Board,Piece,Player,Move,_,_,Length,GameMove) :-
    translateMove(Move,_,Ydif),
    getPiece(Board,_,Yi,Piece),
    Ysum is Yi + Ydif,
    length(Board,Height),
    Ysum > Height,
    nl,nl,write('That move is not valid'),nl,nl,
    gameLoop(Board,Player,GameMove,Length).    


    



    

play:- 
    prompt(_, ''),
    menu.


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
    tabuleiro(Board),
    drawGame(Board,1,1,5),
    gameLoop(Board,1,1,5).

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


winner(Player) :-
    nl,nl,write('                                Player '),write(Player),write(' won'),nl,nl,
    menu.

gameLoop(Board,Player,Move,Length) :- 
    write('Choose your piece: '), 
    get_char(Piece),
    get_char(_),   
    checkInputPiece(Board,Piece,Player,NewPiece,Move,Length),
    write('Choose your move: '), 
    get_char(PlayerMove),
    get_char(_),
    checkInputMove(Board,NewPiece,Player,PlayerMove,X2,Y2,Length,Move),
    movePiece(Board,NewPiece,X2,Y2,Player,Finalboard),
    checkStateEnd(Finalboard,Player),
    changePlayer(Player,NewPlayer),
    incrementMove(Move,NewMove),nl,nl,
    drawGame(Finalboard,NewMove,NewPlayer,Length),
    gameLoop(Finalboard,NewPlayer,NewMove,Length).



