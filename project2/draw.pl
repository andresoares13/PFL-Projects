printBoard([],_) :-
    nl.

printBoard([Row],L):-
    nl,
    printRowFinal(Row,L),
    printBoard([],L).

printBoard([Row|Tail],L) :-
    length([Row|Tail],L2),
    L2>1,
    nl,
    printRow(Row,L),
    printBoard(Tail,L).


printRowFinal(Row,L) :-
    write('                      '),write('\x2503\'),write(' '),
    printRowAux(Row),write('\x2503\'),nl,
    write('                      '),write('\x2517\'),
    printBorderLineFinal(L),write('\x251B\').

printRow(Row,L):-
    write('                      '),write('\x2503\'),write(' '),
    printRowAux(Row),write('\x2503\'),nl,
    write('                      '),write('\x2523\'),
    printBorderLineMiddle(L),write('\x252B\').

printRowAux([]).
printRowAux([E]) :-
    atom_length(E,AL),
    AL < 3,
    write(' '),
    write(E),write('  ').
printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL < 3,
    write(' '),
    write(El),
    write('  '),write('\x2503\'),write(' '),
    printRowAux(Tail).

printRowAux([E]) :-
    atom_length(E,AL),
    AL > 2,
    write(' '),
    write(E).
printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL > 2,
    write(' '),
    write(El),write(' '),
    write('\x2503\'),
    printRowAux(Tail).    

printBorderLineMiddle(0).
printBorderLineMiddle(1):-
    printBorderLineAuxEnd(6),
    printBorderLineMiddle(0).
printBorderLineMiddle(L):-
    L > 1,
    L1 is L - 1,
    printBorderLineAuxMiddle(6),
    printBorderLineMiddle(L1).

printBorderLineFinal(0).
printBorderLineFinal(1):-
    printBorderLineAuxEnd(6),
    printBorderLineFinal(0).
printBorderLineFinal(L) :-
    L > 1,
    L1 is L - 1,
    printBorderLineFinalAux(6),
    printBorderLineFinal(L1).


printBorderLine(0).
printBorderLine(1):-
    printBorderLineAuxEnd(6),
    printBorderLine(0).

printBorderLine(L) :-
    L > 1,
    L1 is L - 1,
    printBorderLineAux(6),
    printBorderLine(L1).

printBorderLineAux(0) :-
    write('\x2533\').
printBorderLineAux(L) :-
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAux(L1).

printBorderLineAuxEnd(0).
printBorderLineAuxEnd(L) :-    
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAuxEnd(L1).


printBorderLineAuxMiddle(0) :-
    write('\x254B\').
printBorderLineAuxMiddle(L) :-
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAuxMiddle(L1).    

printBorderLineFinalAux(0) :-
    write('\x253B\').
printBorderLineFinalAux(L) :-
    L>0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineFinalAux(L1).

drawGame(Board,Move,Player,Length) :-
    nl,nl,nl,
    write('                                   Ugly Duck'),nl,nl,
    write('                                    Move: '),write(Move),nl,nl,
    write('                                  Player No: '),write(Player),nl,nl,
    write('                        '),
    reverse(Board,Boardprint), % the reverse is so that the print can be seen correctly in the console
    nl,write('                      '),write('\x250F\'),
    printBorderLine(Length),write('\x2513\'),
    printBoard(Boardprint,Length),
    write('                        '),
    nl,nl,nl,nl.