%predicate that creates the empty cells in the board, it creates as many rows of empty cells as the height of the board - 2 (for the 2 rows with players pieces)
%createBoardSpaces(+Board,+Length,+Height,-NewBoard)
createBoardSpaces(Board,_,0,Board).
createBoardSpaces(Board,Length,Height,NewBoard) :-
    Height > 0,
    H is Height - 1,
    createRowSpaces([],Length,NewRow),
    append(Board,[NewRow],Board2),
    createBoardSpaces(Board2,Length,H,NewBoard).

%predicate that given a row and its length, fills it with empty cells, uses recursion to know when it has to stop
%createRowSpaces(+Row,+Length,-NewRow)
createRowSpaces(Row,0,Row).
createRowSpaces(Row,Length,NewRow) :-
    Length > 0,
    L is Length - 1,
    append(Row,['  '], Row2),
    createRowSpaces(Row2,L,NewRow).

%predicate that creates player 1, uses recursion to add each element up until the length of the board and when it has no more to add, returns the completed row
%createPlayer1(+Row,+Length,+Counter,-NewRow)
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


%predicate that creates player 2, uses recursion to add each element up until the length of the board and when it has no more to add, returns the completed row
%createPlayer1(+Row,+Length,+Counter,-NewRow)
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


%predicate that given the length and height creates the board, first it creates the first player´s pieces, then the spaces (empty cells) and then the second player
%createBoard(+Length,+Height,-Board)
createBoard(Length,Height,Board) :-
    createPlayer1([],Length,1,NewRow),
    SpacesHeight is Height - 2,
    createBoardSpaces([NewRow],Length,SpacesHeight,NewBoard),
    createPlayer2([],Length,1,NewRow2),
    append(NewBoard,[NewRow2],Board).


%predicate that given an element X, element Y, List and an Index, replaces the element X at the Index in the List by Y and returns NewList
%replace_at_index(+X,+Y,+List,+Index,-NewList)
replace_at_index(X,Y,List,Index,NewList) :-
    replace_at_index(X,Y,List,Index,1,NewList).


%works recursively until it reaches the moment where the index is the same and then it unifies the Y element at the head of the List that is being ´iterated´ upon
replace_at_index(_,_,[],_,_,[]).
replace_at_index(X,Y,[X|Xs],Index,CurrentIndex,[Y|Xs]) :-
    Index =:= CurrentIndex.                                  
replace_at_index(X,Y,[Z|Xs],Index,CurrentIndex,[Z|Zs]) :-
    CurrentIndex < Index,
    NewIndex is CurrentIndex + 1,
    replace_at_index(X,Y,Xs,Index,NewIndex,Zs).


%hybrid predicate that get either given the position given a piece or a piece given the position, it reaches the specified element and makes sure the piece is valid given the player, ex player 1 pieces should start with a D or S
% getPiece(+Board,?X,?Y,?Piece,+Player,?ActualPiece)
getPiece(Board,X,Y,Piece,Player,ActualPiece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,ActualPiece),
    atom_concat(_,Piece,ActualPiece),
    getFirstLetter(ActualPiece,Letter),
    validPiece(Player,Letter).

%predicate that given a board and coordinates gives the piece, note that we need a specific predicate for this (different than the one above) since sometimes we need to get the piece with the letter, which the predicate above does not always achieve, and other times we need the to take advantage of having multiple possibilities for input so we use the one above, really depends on the circumstance
%getPieceByPos(+Board,+X,+Y,-Piece)
getPieceByPos(Board,X,Y,Piece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,Piece).

%predicate that is the reverse of the one above, given an atom, specificaly with the correct letter, we need this so that when we search for element 1, we dont also get elements 11 and 21 that may happen due to the fact that the getPiece is hybrid in nature and allows for multiple results (which again, is sometimes useful). Note that this one is also hybrid in nature
%getPieceByAtom(+board,?X,?X,?Piece,+Player,?ActualPiece)
getPieceByAtom(Board,X,Y,Piece,Player,ActualPiece) :-
    nth1(Y,Board,Line),
    nth1(X,Line,ActualPiece),
    atom_concat(_,Piece,ActualPiece),
    sub_atom(ActualPiece, 1, _, 0, Piece),
    getFirstLetter(ActualPiece,Letter),
    validPiece(Player,Letter).

%predicate that given an atom returns its first letter, super useful when we need to know to which player the piece belongs
%getFirstLetter(+Atom,-FirstLetter)
getFirstLetter(Atom, FirstLetter) :-
    atom_chars(Atom, AtomChars),
    nth1(1, AtomChars, FirstLetter).


%predicates that given a player, return the possible first letters of its pieces, player 1 is always D and S and player 2, d and s
%validPiece(?Player,?Piece)
validPiece(1,'D').
validPiece(1,'S').
validPiece(2,'d').
validPiece(2,'s').

%predicate that given a Board, a position and a piece, returns a new board with that piece in the given position (basically sets the piece there)
%setPiece(+Board,+X,+Y,+Piece,-Newboard)
setPiece(Board,X,Y,Piece,Newboard) :-
    nth1(Y,Board,Line),
    nth1(X,Line,X1),
    replace_at_index(X1,Piece,Line,X,NewLine),
    replace_at_index(Line,NewLine,Board,Y,Newboard).

%predicate that given a board and a position, returns a new board with that position cleared. When we move a piece, the original position of the piece has to be cleared
%clearPiece(+Board,+X,+Y,-Newboard)
clearPiece(Board,X,Y,Newboard) :-
    nth1(Y,Board,Line),
    nth1(X,Line,X1),
    replace_at_index(X1,'  ',Line,X,NewLine),
    replace_at_index(Line,NewLine,Board,Y,Newboard).


%predicate that moves a piece to a given X2 and Y2 and it returns a board, note that the board is called swanboard as it uses another predicate that checks if the duck reached the last row and turned into a swan, if so, the SwanBoard will contain a swan instead of a duck in that position
%movePiece(+Board,+Piece,+X2,+Y2,+Player,-SwanBoard)
movePiece(Board,Piece,X2,Y2,Player,SwanBoard) :-
    getPiece(Board,X,Y,Piece,Player,_),
    setPiece(Board,X2,Y2,Piece,Newboard),
    clearPiece(Newboard,X,Y,Finalboard),
    checkStateSwan(Finalboard,Player,SwanBoard).

%predicate that combines the predicates that check the input and make the move, the move is only made if the input is valid (the input is a move, the move can be made, etc...)
%move(+GameState, +Move, -NewGameState)
move(Board-Piece-Player-Length-Move,PlayerMove,NewBoard) :-
    checkInputMove(Board,Piece,Player,PlayerMove,X2,Y2,Length,Move),
    movePiece(Board,Piece,X2,Y2,Player,NewBoard).

%predicate that given a Piece, changes its first letter to a swan, basically evolving the duck, the letter depends on the player, hence the two predicates where the only thing that changes is the player number
%evolvePiece(+Piece,-Atom,+Player)
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

     
%changes the player, basically a toggle
%changePlayer(+Player,-NewPlayer)    
changePlayer(1,2).
changePlayer(2,1).


%checks if the first letter of the piece has reached the last row, in the case of player 1 it has to reach the height of the board, which is the length of the array (number of rows) and for the player 2 simply reach the first row
%getPlayerSwan(+Player,+Board,+Line,+Letter)
getPlayerSwan(1,Board,Line,'D') :-
    length(Board,Line).

getPlayerSwan(2,_,1,'d').


%checks if the given player has any ducks that have reached their last row and will evolve into swans, if so, returns a newboard with the duck replaced by a swan, or just the board if there are no new swans
%checkStateSwan(+Boardm+Player,-NewBoard)
checkStateSwan(Board,Player,NewBoard) :-
    getPlayerSwan(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    member(M,Line),
    sub_atom(M,0,_,_,Letter),
    nth1(X,Line,M),
    evolvePiece(M,Swan,Player),
    setPiece(Board,X,LineNr,Swan,NewBoard).

checkStateSwan(Board,Player,Board) :- %in case there are no ducks on the last row
    getPlayerSwan(Player,Board,LineNr,Letter),
    nth1(LineNr,Board,Line),
    \+ is_member(Letter,Line).


%checks if the first letter of an element exists in a list, useful when checking for ducks and swans
%is_member(+Element,+List)
is_member(Element, [H|_]) :-
    sub_atom(H, 0, 1, _, Element).

is_member(Element, [_|T]) :-
    is_member(Element, T).

%predicate that confirms the end state, the first letter of the piece (a swan), needs to reach the starting players row, for the 1st player, its row 1, for the 2nd player it depends on the height of the board, similar to the predicate that checks swans
%playerEnd(+Player,+Board,+Line,+Letter)
playerEnd(1,_,1,'S'). 

playerEnd(2,Board,Line,'s') :-
    length(Board,Line).


%see if the player still has any peaces alive, if all of the pieces are gone, the player without pieces loses
%does this by finding all the pieces of a certain player
%hasPieces(+Board,+Player)
hasPieces(Board,Player) :-
    findall(Piece, (
        getPiece(Board,_,_,Piece,Player,_),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter)
    ), PieceList),
    length(PieceList,Len),
    Len > 0.


%predicate that given a board and a player writes (output) the pieces that, that player can move 
%writePossiblePieces(+Board,+Player)
writePossiblePieces(Board,Player) :-
    write('Pieces: '),
    findall(Piece,(
        getPiece(Board,_,_,Piece,Player,Piece),
        sub_atom(Piece, 1, _, 0, PieceNr),
        write(PieceNr),write(', ')
    ),_),
    write('Simply write the number and press enter'),nl,nl.


%predicate that given a Board, a player, the board length and a piece, finds the possible moves for that piece (in the form of a letter like w, q,e,etc )
%it does this with the help of the predicate valid_moves defined in the computer.pl file, it also uses recursion to write every move with an arrow to better tell the move the player is about to make
%writePossibleMoves(+Board,+Player,+Length,+Piece)
writePossibleMoves(Board,Player,Length,Piece) :-
    nl, write('The possible moves for the Piece '),write(Piece),write(' are: '),
    valid_moves(Board-Length-Piece,Player,MoveList),
    writePossibleMovesAux(MoveList),
    write('Simply write the letter and press enter'),nl,nl.

writePossibleMovesAux([]).
writePossibleMovesAux([Move|Rest]) :-
    translateArrowMove(Move),
    writePossibleMovesAux(Rest).    



%checks if the game is over and if so, declares the winner and returns to the menu
%checkStateEnd(+Board,+Player)
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


%writes the player that won (output) and goes to the menu
%winner(+Player)
winner(Player) :-
    nl,nl,write('                                Player '),write(Player),write(' won!'),nl,nl,
    menu.


%increments the Game Move by 1 (game move counter)
%incrementMove(+Move,-NewMove)
incrementMove(Move,NewMove) :-
    NewMove is Move + 1.



%predicate that given the move displays it in a more user friendly way with arrows
%translateArrowMove(+Letter)
translateArrowMove('w') :- write('\x2191\'),write(' w, ').
translateArrowMove('q') :- write('\x2196\'),write(' q, ').
translateArrowMove('e') :- write('\x2197\'),write(' e, ').
translateArrowMove('s') :- write('\x2193\'),write(' s, ').
translateArrowMove('a') :- write('\x2199\'),write(' a, ').
translateArrowMove('d') :- write('\x2198\'),write(' d, ').
