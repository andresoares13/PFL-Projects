writeColor(Element) :-
    getFirstLetter(Element,Letter),
    writeColorAux(Letter,Element).

writeColorAux('D',Element) :-
    format("\e[32m~w\e[0m", [Element]),
    flush_output.

writeColorAux('d',Element) :-
    format("\e[31m~w\e[0m", [Element]),
    flush_output.

writeColorAux('S',Element) :-
    format("\e[32;1m~w\e[0m", [Element]),
    flush_output.

writeColorAux('s',Element) :-
    format("\e[31;1m~w\e[0m", [Element]),
    flush_output.

writeColorAux(' ',Element) :-
    write(Element).


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





%pieces with number 10 or bigger

last_two_digits(Atom, LastTwo) :-
    atom_length(Atom, Length),
    End is Length - 1,
    Start is End - 1,
    sub_atom(Atom, Start, 2, 0, LastTwo).

printRowAux([E]) :-
    atom_length(E,AL),
    AL > 2,
    last_two_digits(E,'10'),
    write(' '),
    writeColor(E),
    write(' ').

printRowAux([E]) :-
    atom_length(E,AL),
    AL > 2,
    last_two_digits(E,Last),
    Last @> '10',
    write('  '),
    writeColor(E),
    write(' ').    
   

printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL > 2,
    last_two_digits(El,Last),
    Last @> '10',
    write('  '),
    writeColor(El),
    write(' '),
    write('\x2503\'),
    printRowAux(Tail).  

printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL > 2,
    write(' '),
    last_two_digits(El,'10'),
    writeColor(El),
    write(' '),
    write('\x2503\'),
    printRowAux(Tail).  

%regular elements

printRowAux([]).
printRowAux([E]) :-
    atom_length(E,AL),
    AL < 3,
    write(' '),
    writeColor(E),
    write('  ').
printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL < 3,
    write(' '),
    writeColor(El),
    write('  '),write('\x2503\'),write(' '),
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

display_game(Board-Move-Player-Length) :-
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