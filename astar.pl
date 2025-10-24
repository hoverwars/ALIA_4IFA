:- module(astar, [iaAStar/3]).

:- use_module(shared).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA PUISSANCE 4 — ALGORITHME A*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iaAStar(Board, Move, Player) :-
    findall(M, coup_possible(Board, M), Moves),

    % Au début, toujours jouer au milieu
    (is_board_empty(Board) ->
        Move = 18, ! ;

    % Si un des coups possible fait gagner, alors on le joue pour gagner
    (member(M, Moves),
     playMove(Board, M, M, B1, Player),
     win(B1, Player)) ->
        Move = M, ! ;

    % Si un des coups possible fait perdre, alors on le joue pour défendre
    (changePlayer(Player, Opp),
     member(M, Moves),
     playMove(Board, M, M, B2, Opp),
     win(B2, Opp)) ->
        Move = M, ! ;

    % Sinon, on choisit le meilleur coup
    best_move_astar(Board, Player, Moves, Move)
    ).


best_move_astar(Board, Player, Moves, BestMove) :-
    % Pour chaque coup possible, évaluer avec A*
    evaluate_moves_astar(Board, Player, Moves, [], ScoredMoves),
    keysort(ScoredMoves, Sorted),
    reverse(Sorted, [Score-BestMove|_]),
    format('Best A* move score: ~w~n', [Score]).

evaluate_moves_astar(_, _, [], Acc, Acc).
evaluate_moves_astar(Board, Player, [Move|Rest], Acc, Out) :-
    playMove(Board, Move, Move, NewBoard, Player),

    % Évaluation heuristique pour A*
    heuristic(NewBoard, Player, HeuristicValue),

    % Coût = 1 par défaut (on pourrait ajuster selon la stratégie)
    Cost = 1,

    % Score = Heuristique - Coût (on maximise l'heuristique et minimise le coût)
    Score is HeuristicValue - Cost,

    evaluate_moves_astar(Board, Player, Rest, [Score-Move|Acc], Out).

heuristic(Board, Player, Score) :-
    % Évaluation basée sur les séquences de pions
    evaluate_sequences(Board, Player, PlayerScore),
    evaluate_sequences(Board, _, OppScore),
    Score is PlayerScore - OppScore.

evaluate_sequences(Board, Player, TotalScore) :-
    findall(Score,
            (between(0, 41, Pos),
             evaluate_position(Board, Player, Pos, Score)),
    Scores),
    sum_list(Scores, TotalScore).

evaluate_position(Board, Player, Pos, Score) :-
    nth0(Pos, Board, Cell),
    (Cell == Player ->
        evaluate_directions(Board, Player, Pos, Score)
    ; Score = 0
    ).

evaluate_directions(Board, Player, Pos, TotalScore) :-
    % Évaluer dans les 4 directions
    evaluate_direction(Board, Player, Pos, 1, 0, Horizontal),   % Horizontal
    evaluate_direction(Board, Player, Pos, 0, 1, Vertical),     % Vertical
    evaluate_direction(Board, Player, Pos, 1, 1, DiagRight),    % Diagonale droite
    evaluate_direction(Board, Player, Pos, 1, -1, DiagLeft),    % Diagonale gauche

    TotalScore is Horizontal + Vertical + DiagRight + DiagLeft.

evaluate_direction(Board, Player, Pos, ColStep, RowStep, Score) :-
    evaluate_line(Board, Player, Pos, ColStep, RowStep, 0, Count),
    line_value(Count, Value),

    % Vérifier aussi la direction opposée
    NegColStep is -ColStep,
    NegRowStep is -RowStep,
    evaluate_line(Board, Player, Pos, NegColStep, NegRowStep, 0, CountOpp),
    line_value(CountOpp, ValueOpp),

    Score is Value + ValueOpp.

evaluate_line(Board, Player, Pos, ColStep, RowStep, CurrentCount, TotalCount) :-
    NextPos is Pos + ColStep * 6 + RowStep,  % Conversion colonne/ligne
    NextPos >= 0, NextPos =< 41,
    nth0(NextPos, Board, Cell),
    (Cell == Player ->
        NewCount is CurrentCount + 1,
        evaluate_line(Board, Player, NextPos, ColStep, RowStep, NewCount, TotalCount)
    ; TotalCount = CurrentCount
    ).

evaluate_line(_, _, _, _, _, Count, Count).

line_value(0, 0).
line_value(1, 1).    % 1 pion isolé
line_value(2, 10).   % 2 pions alignés
line_value(3, 100).  % 3 pions alignés (presque gagnant)
line_value(4, 1000). % 4 pions alignés (victoire)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIN IA PUISSANCE 4 — ALGORITHME A*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%