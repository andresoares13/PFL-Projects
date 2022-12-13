tabuleiro([['X ','D2','D3','D4','D5'],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['d1','d2','d3','d4','d5']]).
:- use_module(library(lists)).
% piece(Type,Player,Pos).
% type can be D or S, Player can be 1 or 2, Pos is [X,Y] where X and Y are between 1 and 5 

printBoard([]) :-
    nl.
printBoard([Row|Tail]) :-
    nl,
    write('                         |  '),
    printRow(Row),
    write('  |'),
    nl,
    printBoard(Tail).

printRow([]).
printRow([E]) :-
    write(E).
printRow([El|Tail]) :-
    write(El),
    write('   '),
    printRow(Tail).

printBorderLine(0).
printBorderLine(L) :-
    L1 is L - 1,
    write(' - '),
    printBorderLine(L1).



drawGame(Board) :-
    nl,nl,nl,
    nth1(1,Board,Line),
    length(Line,Length),
    LineLength is Length * 2,
    write('                        '),
    printBorderLine(LineLength), % prints the border line on top of the board
    reverse(Board,Boardprint), % the reverse is so that the print can be seen correctly in the console
    nl,printBoard(Boardprint),
    write('                        '),printBorderLine(LineLength),
    nl,nl,nl,nl.


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
    replace_at_index(X1,'X ',Line,X,NewLine),
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

convertPiece(Piece,2,NewPiece) :-
    atom_concat('d',Piece,NewPiece).    
    
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
    




checkInputPiece(_,'.',_,_).

checkInputPiece(Board,Piece,Player,NewPiece) :-
    Piece @> '0',
    nth1(1,Board,Line),
    length(Line,Length),
    L1 is Length + 1, % we want to compare if the atom is smaller so we add 1 to length to be able to do '5' @< '6' if the length is 5 for example
    number_codes(L1,[L|_]),
    char_code(Char,L),
    Piece @< Char, % this confirms that the chosen piece is between the possible pieaces given the length of the board which can vary
    convertPiece(Piece,Player,NewPiece), 
    getPiece(Board,_,_,NewPiece). % test to see if the piece is still alive

checkInputPiece(Board,Piece,Player,_) :-
    Piece @< '1',
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    loop(Piece,Board,Player).

checkInputPiece(Board,Piece,Player,_) :-
    nth1(1,Board,Line),
    length(Line,Length),
    number_codes(Length,[L|_]),
    char_code(Char,L),
    Piece @> Char,
    nl,nl,write('Could not find that piece, Please choose an existing piece'),nl,nl,
    loop(Piece,Board,Player).    

checkInputPiece(Board,Piece,Player,_) :-
    convertPiece(Piece,Player,NewPiece), 
    \+ getPiece(Board,_,_,NewPiece),
    nl,nl,write('That piece was captured, Please choose an existing piece'),nl,nl,
    loop(Piece,Board,Player).    


    

go:- 
    prompt(_, ''),
    tabuleiro(Board),
    drawGame(Board),
    loop(start,Board,1),  
    loop('end',Board,1).   

loop(A,Board,Player) :- 
    A\='end',
    write('Player No '),write(Player),nl,
    write('Choose your piece: '), 
    get_char(Piece),
    get_char(_),   
    checkInputPiece(Board,Piece,Player,NewPiece),
    write('Desired position X: '), 
    get_code(X),
    get_char(_),
    write('Desired position Y: '),
    get_code(Y),
    get_char(_),
    convertPos(X,Y,X2,Y2),
    nl,movePiece(Board,NewPiece,X2,Y2,Player,Finalboard),nl,changePlayer(Player,NewPlayer),drawGame(Finalboard),
    loop(X2,Finalboard,NewPlayer).
    




