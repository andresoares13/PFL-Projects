%useful for AI

valid_moves(Board-Length-Piece,Player,MoveList) :-
    getFirstLetter(Piece,Letter),
    findall(Move, (
        validPlayerPieceMove(Player,Letter,Move),
        translateMove(Move,Xdif,Ydif),
        getPiece(Board,Xi,Yi,Piece,Player,_),
        Xsum is Xi + Xdif,
        Xsum > 0,
        Xsum < Length + 1,
        Ysum is Yi + Ydif,
        Ysum > 0,
        length(Board,Height),
        Ysum < Height + 1,
        \+ ownPiece(Board,Xsum,Ysum,Player),
        \+ verticalCapture(Board,Xsum,Ysum,Player,Move)
    ), MoveList).
    
random_piece(Length, N) :-
    PureLength is Length + 1,
    random(1, PureLength, N).



%choose_move(+GameState, +Player,+Level, -Move)
choose_move(Board-Length-Piece, Player, 1, Move) :-
    repeat,
    random_piece(Length,PieceNumber),
    convertNumberToAtom(PieceNumber,Piece),
    getPiece(Board,_,_,Piece,Player,NewPiece),
    valid_moves(Board-Length-NewPiece,Player,MoveList),
    random_member(Move, MoveList),
    !.


computerMove(Board-Length,Player,1,NewBoard) :-
    choose_move(Board-Length-Piece,Player,1,Move),
    translateMove(Move,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard).


%level two of difficulty is a greedy approach opposed to a random one.






% Value predicate, it will given a gamestate, in this case we give the board, 
% determine the value of that board for that player
% %points are given for each more piece than opponent, for example, 50 points 
% for each swan that the player has over the opponent and 20 for each duck, 5 points for being closer to the line (depends on being a duck or swan)
% 1000 points if the next move wins the game

%value(+GameState, +Player, -Value)
value(Board, Player, Value) :-    
    swanCount(Board,Player,Player1SwanCount),
    changePlayer(Player,OtherPlayer),
    swanCount(Board,OtherPlayer,Player2SwanCount),
    SwanPoints is Player1SwanCount - Player2SwanCount,
    duckCount(Board,Player,Player1DuckCount),
    duckCount(Board,Player,Player2DuckCount),
    DuckPoints is Player1DuckCount - Player2DuckCount,
    Value is SwanPoints + DuckPoints.


%The next two functions search the whole board to find ducks and swans. We need to know this to calculate the value of a given board.

%swanCount(Board,Player,Count)
swanCount(Board,Player,Count) :-
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        isSwan(Player,Letter)
    ),Swans),
    length(Swans,Count).

%duckCount(Board,Player,Count)
duckCount(Board,Player,Count) :-
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        isDuck(Player,Letter)
    ),Ducks),
    length(Ducks,Count).


isSwan(1,'S').
isSwan(2,'s').
isDuck(1,'D').
isDuck(2,'d').


tryMove(Board-Piece,Moves,Move-Val) :-
    head(Moves,H),
    tryMoveAux(Board-Piece,Moves,H-0,Move-Val).

tryMoveAux(_,[],Move-Acc,Move-Acc).
tryMoveAux(Board-Piece,[TMove|Rest],_-Acc,Move-Val) :-
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value > Acc,
    tryMoveAux(Board-Piece,Rest,TMove-Value,Move-Val).

tryMoveAux(Board-Piece,[TMove|Rest],AMove-Acc,Move-Val) :-
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value =< Acc,
    tryMoveAux(Board-Piece,Rest,AMove-Acc,Move-Val).


bestMoveByPiece(Board-Length,Player,Moves) :-
    findall(Piece-Move-Value,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        valid_moves(Board-Length-Piece,Player,Moves),
        tryMove(Board-Piece,Moves,Move-Value)
    ),Moves).



maxTupleMoveValue(Moves,Piece-Move-Max) :-
    head(Moves,FPiece-FMove-FVal),
    maxTupleMoveValueAux(Moves,FPiece-FMove-FVal,Piece-Move-Max).

maxTupleMoveValueAux([],Piece-Move-Acc,Piece-Move-Acc).
maxTupleMoveValueAux([TPiece-TMove-Value|Rest],_-_-Acc,Piece-Move-Max) :-
    Value > Acc,
    maxTupleMoveValueAux(Rest,TPiece-TMove-Value,Piece-Move-Max).

maxTupleMoveValueAux([_-_-Value|Rest],APiece-AMove-Acc,Piece-Move-Max) :-
    Value =< Acc,
    maxTupleMoveValueAux(Rest,APiece-AMove-Acc,Piece-Move-Max).
