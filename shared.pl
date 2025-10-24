:- module(shared, [replace_nth0/5, coup_possible/2, existe_case_libre/2, is_board_empty/1]).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   % predicate works forward: Index,List -> OldElem, Transfer
   nth0(Index,List,OldElem,Transfer),
   % predicate works backwards: Index,NewElem,Transfer -> NewList
   nth0(Index,NewList,NewElem,Transfer).

coup_possible(Board, ColStart) :-
    member(ColStart, [0,6,12,18,24,30,36]),
    nth0(ColStart, Board, _),    % colonne existe
    existe_case_libre(Board, ColStart).

existe_case_libre(Board, ColStart) :-
    End is ColStart + 5,
    End =< 41,
    between(ColStart, End, I),
    nth0(I, Board, C),
    C == '_', !.

is_board_empty(Board) :- \+ (member(Cell, Board), Cell \= '_').
