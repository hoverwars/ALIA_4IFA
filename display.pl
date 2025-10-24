:- module(display, [displayBoard/1]).

% Replace empty cells for display clarity
replace_empty('_', '.') :- !.
replace_empty('', '.') :- !.
replace_empty(X, X).

displayBoard(Board) :-
    maplist(replace_empty, Board, CleanBoard),
    NumCols = 7,
    NumRows = 6,
    writeln(' 0   6   12  18  24  30  36'),
    display_rows(CleanBoard, NumCols, NumRows, 0).

display_rows(_, _, NumRows, NumRows) :- !.
display_rows(Board, NumCols, NumRows, RowIndex) :-
    print_row(Board, NumCols, NumRows, RowIndex),
    NextRow is RowIndex + 1,
    display_rows(Board, NumCols, NumRows, NextRow).

print_row(Board, NumCols, NumRows, RowIndex) :-
    MaxCol is NumCols - 1,                       % evaluate NumCols-1
    % For each column compute the element index in column-major order
    findall(Cell,
            ( between(0, MaxCol, Col),
              Index is Col * NumRows + RowIndex,
              nth0(Index, Board, Cell)
            ),
            Row),
    print_cells(Row),
    nl.

print_cells([]).
print_cells([C|Cs]) :-
    format(' ~w |', [C]),
    print_cells(Cs).