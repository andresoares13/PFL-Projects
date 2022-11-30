tabuleiro([['D1','D2','D3','D4','D5'],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['D6','D7','D8','D9','D10']]).
:- use_module(library(lists)).
% piece(Type,Player,Pos).
% type can be D or S, Player can be 1 or 2, Pos is [X,Y] where X and Y are between 1 and 5 

printBoard([]) :-
    nl.
printBoard([Row|Tail]) :-
    nl,
    printRow(Row),
    nl,
    printBoard(Tail).

printRow([]).
printRow([El|Tail]) :-
    write(El),
    write(' '),
    printRow(Tail).    

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
    I > -1, 
    NI is I-1, 
    replace(T, NI, X, R), !.
replace(L, _, _, L).        

% getPiece(-Board,-X,-Y,+Piece)
getPiece(Board,X,Y,Piece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,Piece).


setPiece(Board,X,Y,Piece,Newboard) :-
    nth1(Y,Board,Line),
    X1 is X-1,
    Y1 is Y-1,
    replace(Line,X1,Piece,Newline),
    replace(Board,Y1,Newline,Newboard).

clearPiece(Board,X,Y,Newboard) :-
    nth1(Y,Board,Line),
    X1 is X-1,
    Y1 is Y-1,
    replace(Line,X1,'X ',Newline),
    replace(Board,Y1,Newline,Newboard).

movePiece(Board,X,Y,X2,Y2,Finalboard) :-
    getPiece(Board,X,Y,Piece),
    setPiece(Board,X2,Y2,Piece,Newboard),
    clearPiece(Newboard,X,Y,Finalboard),
    reverse(Finalboard,Boardprint), % the reverse is so that the print can be seen correctly in the console
    printBoard(Boardprint), !.



    
