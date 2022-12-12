tabuleiro([['D1','D2','D3','D4','D5'],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['X ','X ','X ','X ','X '],['d1','d2','d3','d4','d5']]).
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

movePiece(Board,Piece,X2,Y2,Finalboard) :-
    getPiece(Board,X,Y,Piece),
    setPiece(Board,X2,Y2,Piece,Newboard),
    clearPiece(Newboard,X,Y,Finalboard),
    reverse(Finalboard,Boardprint), % the reverse is so that the print can be seen correctly in the console
    printBoard(Boardprint).



convertPos(X,Y,X2,Y2) :-
    number_codes(X2,[X]),
    number_codes(Y2,[Y]).

convertPiece(Piece,1,NewPiece) :-
    atom_concat('D',Piece,NewPiece).

convertPiece(Piece,2,NewPiece) :-
    atom_concat('d',Piece,NewPiece).    
    
changePlayer(1,2).
changePlayer(2,1).    
    

go:- 
    prompt(_, ''),
    tabuleiro(Board),
    reverse(Board,Temp),
    printBoard(Temp),nl ,
    loop(start,Board,1),  
    loop(end,Board,1).   
loop(A,Board,Player) :- 
    A\=end,
    write('Player No '),write(Player),nl,
    write('Choose your piece: '), 
    get_char(Piece),
    get_char(_),   
    write('Desired position X: '), 
    get_code(X),
    get_char(_),
    write('Desired position Y: '),
    get_code(Y),
    get_char(_),
    convertPos(X,Y,X2,Y2),
    convertPiece(Piece,Player,NewPiece), 
    nl,movePiece(Board,NewPiece,X2,Y2,Newboard),nl,changePlayer(Player,NewPlayer) ,loop(X2,Newboard,NewPlayer).  
