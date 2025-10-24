:- module(game, [playMove/5, changePlayer/2, win/2]).

:- use_module(shared).
:- dynamic board/1. % permet l'assertion et le retrait de faits board/1

verifyCell(Board,Player,Pos):-
    nth0(Pos, Board, Cell),
    Cell == Player.

winRow(Board, Player, X):-
    Index1 is X + 6,
    Index2 is X + 12,
    Index3 is X + 18,
    verifyCell(Board, Player, X),
    verifyCell(Board, Player, Index1),
    verifyCell(Board, Player, Index2),
    verifyCell(Board, Player, Index3).
    % format('Victoire en ligne pour X = ~w~n', [X]).

winRow(Board, Player, X):-
    X < 24,
    Index1 is X + 1,
    winRow(Board, Player, Index1).

winColumn(Board, Player, X):-
    Index1 is X + 1,
    Index2 is X + 2,
    Index3 is X + 3,
    verifyCell(Board, Player, X),
    verifyCell(Board, Player, Index1),
    verifyCell(Board, Player, Index2),
    verifyCell(Board, Player, Index3).
    % format('Victoire en colonne pour X = ~w~n', [X]).

winColumn(Board, Player, X):-
    X mod 6 < 2,
    Index1 is X + 1,
    winColumn(Board, Player, Index1).

winColumn(Board, Player, X):-
    X mod 6 > 1,
    X < 36,
    Q is X // 6,
    NewIndex is Q * 6 + 6,
    winColumn(Board, Player, NewIndex).

winRightDiag(Board, Player, X):-
    Index1 is X + 7,
    Index2 is X + 14,
    Index3 is X + 21,
    verifyCell(Board, Player, X),
    verifyCell(Board, Player, Index1),
    verifyCell(Board, Player, Index2),
    verifyCell(Board, Player, Index3).
    % format('Victoire en diagonale bas droite pour X = ~w~n', [X]).

winRightDiag(Board, Player, X):-
    X mod 6 < 2,
    Index1 is X + 1,
    winRightDiag(Board, Player, Index1).

winRightDiag(Board, Player, X):-
    X mod 6 > 1,
    X < 18,
    Q is X // 6,
    NewIndex is Q * 6 + 6,
    winRightDiag(Board, Player, NewIndex).

winLeftDiag(Board, Player, X):-
    Index1 is X - 5,
    Index2 is X - 10,
    Index3 is X - 15,
    verifyCell(Board, Player, X),
    verifyCell(Board, Player, Index1),
    verifyCell(Board, Player, Index2),
    verifyCell(Board, Player, Index3).
    % format('Victoire en diagonale bas gauche pour X = ~w~n', [X]).

winLeftDiag(Board, Player, X):-
    X mod 6 < 2,
    Index1 is X + 1,
    winLeftDiag(Board, Player, Index1).

winLeftDiag(Board, Player, X):-
    X mod 6 > 1,
    X < 36,
    Q is X // 6,
    NewIndex is Q * 6 + 6,
    winLeftDiag(Board, Player, NewIndex).

win(Board, Player):- winRow(Board, Player, 0).
win(Board, Player):- winColumn(Board, Player, 0).
win(Board, Player):- winRightDiag(Board, Player, 0).
win(Board, Player):- winLeftDiag(Board, Player, 18).

playMove(Board,Move,OriginalMove,NewBoard,Player):-
    Diff is Move - OriginalMove,
    Diff < 6,  % Not leaving current column
    NextMove is Move + 1,
    nth0(Move, Board, Cell), Cell == '_',
    playMove(Board, NextMove, OriginalMove, NewBoard, Player).

playMove(Board,Move, OriginalMove, NewBoard, Player) :-
    CurrentMove is Move - 1,
    CurrentMove >= OriginalMove,
    % writeln(CurrentMove),
    replace_nth0(Board, CurrentMove, '_', Player, NewBoard).

changePlayer('x','o').
changePlayer('o','x').