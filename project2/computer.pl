%useful for AI

%predicate that given a gamestate with board, length and piece, and also a player, returns the list with the moves (letters like w,q,e,) that the player can make
%uses a findall to see all the moves that make the piece stay in bounds, are valid for the player, are not towards a piece of the same player and are not a capture vertically, pieces can only capture diagonaly
%valid_moves(+GameState,+Player,-ListOfMoves)
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
    
%gives a random number given the length of the board, we need this to randomly select a piece for AI easy
%random_piece(+Length,-N)
random_piece(Length, N) :-
    PureLength is Length + 1,
    random(1, PureLength, N).


%predicate that given a gamestate with a board, length, piece, player and AI level, gives a random move
%note that we use a repeat since in our game, sometimes the piece chosen may not have a possible move, for example if it has own pieces above it, and in this case we need to select a piece again until we can move it, we select a piece randomly and then a move randomly for true randomness
%this is our only cut in the whole project, but we really need this repeat to end (not fail), when we find a piece that can move and choose the move
%choose_move(+GameState, +Player,+Level, -Move)
choose_move(Board-Length-Piece, Player, 1, Move) :-
    repeat,
    random_piece(Length,PieceNumber),
    convertNumberToAtom(PieceNumber,Piece),
    getPiece(Board,_,_,Piece,Player,NewPiece),
    valid_moves(Board-Length-NewPiece,Player,MoveList),
    random_member(Move, MoveList),
    !.



%this predicate receives a gamestate, a player, a Ai Level and returns a move, this one is used for AI level 1 and it chooses a move randomly, and executes it
%computerMove(GameState,Player,Level,NewBoard)
computerMove(Board-Length,Player,1,NewBoard) :-
    choose_move(Board-Length-Piece,Player,1,Move),
    translateMove(Move,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard).


%level two of difficulty is a greedy approach opposed to a random one.
%in this approach we calculate the list of the best move of each piece (by a value), then get the piece and move with highest value
%sometimes all moves have the same value so in that case the greedy approach is to try and move a swan since it is what ends the game as fast as possible (more value)
computerMove(Board-Length,Player,2,NewBoard) :-
    bestMoveByPiece(Board-Length,Player,MoveTuples), %gets all the best moves by each piece
    maxTupleMoveValue(MoveTuples,Piece-Move-_), %gest the best move from the best moves
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
%we also consider if the temporary board contains a piece of the given player that can be taken
%we dont want the AI to move a piece to a cell only to be captured on the next move, so we give -5 points for each piece that can be captured
%note that pieces are given an important value, since a greedy player would try to get a piece if possible as it reduces the changes of defeat and also you can win this game by capturing all of the opponents pieces

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
    nextMoveCaptured(Board,Player,Captured),
    nextMoveEnds(Board,Player,Ending), 
    Value is SwanPointsMul + DuckPointsMul + Ending + Captured.


%The next two predicates search the whole board to find ducks and swans. We need to know this to calculate the value of a given board.

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

%checks if the letter corresponds to a swan and a duck given the player

%isSwan(+Player,+Letter)
isSwan(1,'S').
isSwan(2,'s').

%isDuck(+Player,+Letter)
isDuck(1,'D').
isDuck(2,'d').


%predicate that simulates every move given in the list Moves, it will then return a tuple Move-Val of the Move and its Value, note that this is for a specific piece
%tryMove(+GameState,+Player,+Moves,-Move-Val)
tryMove(Board-Piece,Player,Moves,Move-Val) :-
    head(Moves,H),
    tryMoveAux(Board-Piece,Player,Moves,H-(-100000),Move-Val). %we need a small value, something like Int_Min in c++, since we need to get the maximum even with negative values


%aux predicate to go through every move a piece can make, simulates it, and then always stores the one with the highest value (values can be negative), so in the end we know what the best move is for a piece (then later in another predicate (maxTupleMoveValue) we calculate out of those best of each piece, the best overall)
%tryMoveAux(+GameState,+Player,+Moves,+MoveAcc-ValAcc,-Move-Val)
tryMoveAux(_,_,[],Move-Acc,Move-Acc). %base case, we just return the Move-Acc
tryMoveAux(Board-Piece,Player,[TMove|Rest],_-Acc,Move-Val) :- %in the case the value is bigger we use it to call the predicate again
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value > Acc,
    tryMoveAux(Board-Piece,Player,Rest,TMove-Value,Move-Val).

tryMoveAux(Board-Piece,Player,[TMove|Rest],AMove-Acc,Move-Val) :- %in case the value is smaller, we simply call the predicate with the same accumulator
    translateMove(TMove,Xdif,Ydif),
    getPiece(Board,Xi,Yi,Piece,Player,NewPiece),
    Xsum is Xi + Xdif,
    Ysum is Yi + Ydif,
    movePiece(Board,NewPiece,Xsum,Ysum,Player,NewBoard),
    value(NewBoard,Player,Value),
    Value =< Acc,
    tryMoveAux(Board-Piece,Player,Rest,AMove-Acc,Move-Val).

%finds the best move for each piece as returns a list with the tuples Piece-Move-Value, uses a find all to do this for every piece of a given player
%bestMoveByPiece(+GameState,+Player,-Moves)
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
%maxTupleMoveValue(+Moves,-MaxValTuple)
maxTupleMoveValue(Moves,Piece-Move-Max) :-
    head(Moves,FPiece-FMove-FVal),
    maxTupleMoveValueAux(Moves,FPiece-FMove-FVal,Piece-Move-Max).


%aux predicate that goes through each Piece-Move-Value tuple and finds the element with highest Value
%maxTupleMoveValueAux(+Moves,+TupleAcc,-TupleMax) The accumulator is used to store the best one, and it is replaced if a better one is found
maxTupleMoveValueAux([],Piece-Move-Acc,Piece-Move-Acc). %in the base case we simply return the Tuple that was left in the accumulator
maxTupleMoveValueAux([TPiece-TMove-Value|Rest],_-_-Acc,Piece-Move-Max) :-
    Value > Acc,
    maxTupleMoveValueAux(Rest,TPiece-TMove-Value,Piece-Move-Max).

maxTupleMoveValueAux([_-_-Value|Rest],APiece-AMove-Acc,Piece-Move-Max) :-
    Value =< Acc,
    maxTupleMoveValueAux(Rest,APiece-AMove-Acc,Piece-Move-Max).


%predicate that checks if the given board (simulated board from a move) is at a game-over state, if so the Ending will have a value of 1000 so it will always be the highest and therefore always played, if the game can be ended, the AI will do it (it is greedy)
%nextMoveEnds(+Board,+Player,-Ending)
nextMoveEnds(Board,Player,Ending) :-
    nextMoveEndsAux(Board,Player,0,Ending).

nextMoveEndsAux(Board,Player,Acc,Ending) :- %game is over, Ending is 1000
    game_over(Board-Player,Player),
    Ending is Acc + 1000.

nextMoveEndsAux(Board,Player,Acc,Acc) :- %game is not over, ending is 0
    \+game_over(Board-Player,Player).


%similar to nextMoveEnds but this one checks if given a board, the Player has any pieces that can be captured by the opponent
%nextMoveCaptured(+Board,+Player,-Captured)
nextMoveCaptured(Board,Player,Captured) :-
    nextMoveCapturedAux(Board,Player,0,Captured).

%aux predicate that sees, whether the Captured is going to be -5 or 0, on the first case, it is near an enemy piece, on the other it is not
%nextMoveCapturedAux(+Board,+Player,+Acc,-Captured)
nextMoveCapturedAux(Board,Player,Acc,Captured) :- 
    nearEnemyPiece(Board,Player),
    Captured is Acc - 5.

nextMoveCapturedAux(Board,Player,Acc,Acc) :-
    \+nearEnemyPiece(Board,Player).


%uses a findall to see if any of the elements in the board are pieces of the given player, and of those if any have pieces that are diagonaly close to any piece from the other player
%the 4 diagonals (or moves) would be q,e,a, and d, hence the 4 possibilities, this is very important as it will tell the AI to avoid moves that allow the opponent to then capture a piece
%nearEnemyPiece(+Board,+Player)
nearEnemyPiece(Board,Player) :-
    changePlayer(Player,OtherPlayer),
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        getPiece(Board,X,Y,Piece,Player,Piece),
        X2 is X + 1,
        Y2 is Y + 1,
        getPieceByPos(Board,X2,Y2,EnemyPiece), %the second player has a piece diagonaly to the 1st players piece
        getFirstLetter(EnemyPiece,LetterEnemy),
        validPiece(OtherPlayer,LetterEnemy)
    ),Player1Pieces),
    length(Player1Pieces,Len),
    Len > 0.

nearEnemyPiece(Board,Player) :-
    changePlayer(Player,OtherPlayer),
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        getPiece(Board,X,Y,Piece,Player,Piece),
        X2 is X - 1,
        Y2 is Y + 1,
        getPieceByPos(Board,X2,Y2,EnemyPiece),
        getFirstLetter(EnemyPiece,LetterEnemy),
        validPiece(OtherPlayer,LetterEnemy)
    ),Player1Pieces),
    length(Player1Pieces,Len),
    Len > 0.

nearEnemyPiece(Board,Player) :-
    changePlayer(Player,OtherPlayer),
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        getPiece(Board,X,Y,Piece,Player,Piece),
        X2 is X - 1,
        Y2 is Y - 1,
        getPieceByPos(Board,X2,Y2,EnemyPiece),
        getFirstLetter(EnemyPiece,LetterEnemy),
        validPiece(OtherPlayer,LetterEnemy)
    ),Player1Pieces),
    length(Player1Pieces,Len),
    Len > 0.

nearEnemyPiece(Board,Player) :-
    changePlayer(Player,OtherPlayer),
    findall(Piece,(
        nth1(_,Board,Line),
        nth1(_,Line,Piece),
        getFirstLetter(Piece,Letter),
        validPiece(Player,Letter),
        getPiece(Board,X,Y,Piece,Player,Piece),
        X2 is X + 1,
        Y2 is Y - 1,
        getPieceByPos(Board,X2,Y2,EnemyPiece),
        getFirstLetter(EnemyPiece,LetterEnemy),
        validPiece(OtherPlayer,LetterEnemy)
    ),Player1Pieces),
    length(Player1Pieces,Len),
    Len > 0.


%checks to see if all of the moves have the same value, in this case we need to see if we have a swan and move it, since that will have more value
%areMovesSameValue(+Moves,+Player,+CurrentMove,-NewMove)
%note that the CurrentMove is the move already chosen by other predicates as the best, we just need to see if it is truly the best should the values be all the same
areMovesSameValue(Moves,Player,_,NewMove) :- %if they have the same value, tries to find a swan and focus on moving it, moving a swan is more valuable
    sameValue(Moves),                         %here we use _ for the CurrentMove as we do not need it since we are going to change it (We dont like Singleton Variables)
    findSwan(Moves,Player,NewMove).


areMovesSameValue(Moves,_,CurrentMove,CurrentMove) :-  %if they do not all contain the same value we simply return the current move
    \+sameValue(Moves).


%predicate that given a list of moves (Piece-Move-Value), sees if the Values are all the same
%sameValue(+Moves)
sameValue(Moves) :-
    head(Moves,M),
    sameValueAux(Moves,M).

%sameValueAux(+Moves,AccMove)
sameValueAux([],_-_-_).
sameValueAux([_-_-Value|Rest],_-_-Value) :- %they all only have the same value, if the temp Value is able to unify with the Value in the accumulator, so we dont need to complicate and this way it will know whether they have the same value or not
    sameValueAux(Rest,_-_-Value).


%predicate that sees if there is a swan among the Pieces of the Moves (Moves is a tuple Piece-Move-Value)
%findSwan(+Moves,+Player,-New) %if there is a swan, we will return that move as it will be the one chosen
findSwan(Moves,Player,New) :-
    head(Moves,Acc),
    findSwanAux(Moves,Player,Acc,New).

%aux predicate to go through each move and find a swan, the accumulator already has the first move
%findSwanAux(+Moves,+Player,+Acc,-New)
findSwanAux([],_,Acc,Acc). %if there were no swans we simply return the accumulator which is the first move which would be the one chosen anyways
findSwanAux([TPiece-TMove-_|_],Player,_,TPiece-TMove-_) :- %if it is a swan, we can stop and return that tuple
    getFirstLetter(TPiece,Letter),
    isSwan(Player,Letter).

findSwanAux([TPiece-_-_|Rest],Player,APiece-AMove-_,New) :- %if it is not a swan we keep going
    getFirstLetter(TPiece,Letter),
    \+isSwan(Player,Letter),
    findSwanAux(Rest,Player,APiece-AMove-_,New).