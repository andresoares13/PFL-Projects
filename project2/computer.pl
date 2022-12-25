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


computerMove(Board-Length,Player,2,NewBoard) :-
    bestMoveByPiece(Board-Length,Player,MoveTuples), %gets all the best moves by each piece
    maxTupleMoveValue(MoveTuples,Piece-Move-_), %gest the best move from the best moves
    write(MoveTuples),nl,flush_output,
    areMovesSameValue(MoveTuples,Player,Piece-Move-_,ChosenP-ChosenM-_), %if all the moves have the same value, then it takes into account other metrics like swan existance and changes the chosen move, ex: if they all have the same value but player 1 has a swan than the swan should move
    translateMove(ChosenM,Xdif,Ydif),
    getPiece(Board,Xi,Yi,ChosenP,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard).




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
    SwanPointsMul is SwanPoints * 50,
    duckCount(Board,Player,Player1DuckCount),
    duckCount(Board,OtherPlayer,Player2DuckCount),
    DuckPoints is Player1DuckCount - Player2DuckCount,
    DuckPointsMul is DuckPoints * 20,
    nextMoveEnds(Board,Player,Ending), 
    Value is SwanPointsMul + DuckPointsMul + Ending.


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


tryMove(Board-Piece,Player,Moves,Move-Val) :-
    head(Moves,H),
    tryMoveAux(Board-Piece,Player,Moves,H-(-100000),Move-Val). %we need a small value, something like Int_Min in c++, since we need to get the maximum even with negative values

tryMoveAux(_,_,[],Move-Acc,Move-Acc).
tryMoveAux(Board-Piece,Player,[TMove|Rest],_-Acc,Move-Val) :-
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value > Acc,
    tryMoveAux(Board-Piece,Player,Rest,TMove-Value,Move-Val).

tryMoveAux(Board-Piece,Player,[TMove|Rest],AMove-Acc,Move-Val) :-
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value =< Acc,
    tryMoveAux(Board-Piece,Player,Rest,AMove-Acc,Move-Val).


bestMoveByPiece(Board-Length,Player,Moves) :-
    findall(Piece-Move-Value,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        valid_moves(Board-Length-Piece,Player,Moves),
        tryMove(Board-Piece,Player,Moves,Move-Value)
    ),Moves).




%find the tuple Piece-Move-Value with the biggest value
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



nextMoveEnds(Board,Player,Ending) :-
    nextMoveEndsAux(Board,Player,0,Ending).

nextMoveEndsAux(Board,Player,Acc,Ending) :-
    game_over(Board-Player,Player),
    Ending is Acc + 1000.

nextMoveEndsAux(Board,Player,Acc,Acc) :-
    \+game_over(Board-Player,Player).




%checks to see if all of the moves have the same value, in this case we need to see if we have a swan and move it, since that will have more value



areMovesSameValue(Moves,Player,_,NewMove) :- %if they have the same value, tries to find a swan and focus on moving it, moving a swan is more valuable
    sameValue(Moves),
    findSwan(Moves,Player,NewMove).


areMovesSameValue(Moves,_,CurrentMove,CurrentMove) :-
    \+sameValue(Moves).


sameValue(Moves) :-
    head(Moves,M),
    sameValueAux(Moves,M).


sameValueAux([],_-_-_).
sameValueAux([_-_-Value|Rest],_-_-Value) :-
    sameValueAux(Rest,_-_-Value).



findSwan(Moves,Player,New) :-
    head(Moves,Acc),
    findSwanAux(Moves,Player,Acc,New).


findSwanAux([],_,Acc,Acc).
findSwanAux([TPiece-TMove-_|_],Player,_,TPiece-TMove-_) :-
    getFirstLetter(TPiece,Letter),
    isSwan(Player,Letter).

findSwanAux([TPiece-_-_|Rest],Player,APiece-AMove-_,New) :-
    getFirstLetter(TPiece,Letter),
    \+isSwan(Player,Letter),
    findSwanAux(Rest,Player,APiece-AMove-_,New).