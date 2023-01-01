%predicate that allow to write the elements with different colors 
%writeColor(+Element)
writeColor(Element) :-
    getFirstLetter(Element,Letter),
    writeColorAux(Letter,Element).

%aux predicate that allows different color options, for example, red, green, bold red and bold green
%writeColorAux(+Letter,+Element)
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

%predicate that prints the board with the specified length, uses recursion to go through the board
%printBoard(+Board,+Length)
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

%receives a row and prints it, in this case it is the final row specifically, because we need specific unicode characters for the bottom row
%printRowFinal(+Row,+Length)
printRowFinal(Row,L) :-
    write('                      '),write('\x2503\'),write(' '),
    printRowAux(Row),write('\x2503\'),nl,
    write('                      '),write('\x2517\'),
    printBorderLineFinal(L),write('\x251B\').

%prints an average row
%printRow(+Row,+Length)
printRow(Row,L):-
    write('                      '),write('\x2503\'),write(' '),
    printRowAux(Row),write('\x2503\'),nl,
    write('                      '),write('\x2523\'),
    printBorderLineMiddle(L),write('\x252B\').


%pieces with number 10 or bigger, we need to specify this because their atoms have 1 more of length

%aux predicate that goes through the row and prints its elements
%printRowAux(+Row)
printRowAux([E]) :-
    atom_length(E,AL),
    AL > 2,
    writeColor(E),
    write('  ').    
   

printRowAux([El|Tail]) :-
    length([El|Tail],L),
    L > 1,
    atom_length(El,AL),
    AL > 2,
    writeColor(El),
    write('  '),
    write('\x2503\'),write(' '),
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


  



%printBorderLineMiddle(+Lines)   used to print the line in the middle of the cells
printBorderLineMiddle(0).
printBorderLineMiddle(1):-
    printBorderLineAuxEnd(6),
    printBorderLineMiddle(0).
printBorderLineMiddle(L):-
    L > 1,
    L1 is L - 1,
    printBorderLineAuxMiddle(6),
    printBorderLineMiddle(L1).


%printBorderLineFinal(+Lines)   used to print the line in the end of the cells
printBorderLineFinal(0).
printBorderLineFinal(1):-
    printBorderLineAuxEnd(6),
    printBorderLineFinal(0).
printBorderLineFinal(L) :-
    L > 1,
    L1 is L - 1,
    printBorderLineFinalAux(6),
    printBorderLineFinal(L1).

%printBorderLine(+Lines)   used to print the lines along the boards
printBorderLine(0).
printBorderLine(1):-
    printBorderLineAuxEnd(6),
    printBorderLine(0).

printBorderLine(L) :-
    L > 1,
    L1 is L - 1,
    printBorderLineAux(6),
    printBorderLine(L1).

%aux predicate to print the lines given acc value
%printBorderLineAux(+Lines)
printBorderLineAux(0) :-
    write('\x2533\').
printBorderLineAux(L) :-
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAux(L1).


%aux predicate to print the lines given acc value, in the end of the table
%printBorderLineAuxEnd(+Lines)
printBorderLineAuxEnd(0).
printBorderLineAuxEnd(L) :-    
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAuxEnd(L1).

%aux predicate to print the lines given acc value, in the middle of the table
%printBorderLineAuxMiddle(+Lines)
printBorderLineAuxMiddle(0) :-
    write('\x254B\').
printBorderLineAuxMiddle(L) :-
    L > 0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineAuxMiddle(L1).    

%aux predicate for the final line
%printBorderLineFinalAux(+Line)
printBorderLineFinalAux(0) :-
    write('\x253B\').
printBorderLineFinalAux(L) :-
    L>0,
    L1 is L - 1,
    write('\x2501\'),
    printBorderLineFinalAux(L1).

%predicate that given a gameState containing the Board, the current move and player to move, and board length, shows that information in a user friendly way
%display_game(+GameState)
display_game(Board-Move-Player-Length) :-
    nl,nl,nl,
    write('                                   Ugly Duck'),nl,nl,
    write('                                    Move: '),write(Move),nl,nl,
    write('                           Current Turn - Player No: '),write(Player),nl,nl,
    write('                        '),
    reverse(Board,Boardprint), % the reverse is so that the print can be seen correctly in the console
    nl,write('                      '),write('\x250F\'),
    printBorderLine(Length),write('\x2513\'),
    printBoard(Boardprint,Length),
    nl,nl,nl,nl,flush_output.