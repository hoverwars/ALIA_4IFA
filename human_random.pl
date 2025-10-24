:- module(human_random, [iaRandom/3, human/3]).

:- use_module(shared).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA Random
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iaRandom(Board, Move, _):-
    findall(Index,
        (nth0(Index, Board, Cell), Cell == '_'), EmptyPositions),
    random_member(RawMove, EmptyPositions),
    Move is RawMove - RawMove mod 6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fin IA Random
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Human
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

human(Board, Move, _) :-
    writeln('Your turn! Choose a column (0-6):'),
    read(Col),
    integer(Col), Col >= 0, Col =< 6, !,
    Move is Col * 6,  % convert column to starting index
    coup_possible(Board, Move), !.
human(Board, Move, Player) :-
    writeln('Invalid move, try again.'),
    human(Board, Move, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fin Human
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%