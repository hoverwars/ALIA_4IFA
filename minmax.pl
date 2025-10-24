:- module(minmax, [iaMinMax/3]).

:- use_module(shared).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA — MINIMAX AVEC ALPHA-BETA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iaMinMax(Board, Move, Player) :-
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

    % Sinon, on choisit le meilleur coup possible.
    best_move(Board, Player, Moves, Move)
    ).

eval_board(Board, Player, Score) :-
    ( win(Board, Player) -> Score = 1000
    ; changePlayer(Player, Opp),
      win(Board, Opp) -> Score = -1000
    ; count_occ(Board, Player, Np),
      changePlayer(Player, Opp),
      count_occ(Board, Opp, No),
      Score is Np - No).

count_occ(Board, Player, N) :-
    include(=(Player), Board, L),
    length(L, N).

best_move(Board, Player, Moves, BestMove) :-
    Alpha = -10000, Beta = 10000, Depth = 4,
    evaluate_moves(Board, Player, Moves, Depth, Alpha, Beta, [], ScoredMoves),

    % Afficher les scores
    forall(member(Score-M, ScoredMoves),
           format('(MinMax) Move ~w -> Score ~w~n', [M, Score])),
    
    keysort(ScoredMoves, Sorted),
    reverse(Sorted, [_-BestMove|_]).

evaluate_moves(_, _, [], _, _, _, Acc, Acc).
evaluate_moves(Board, Player, [Move|Rest], Depth, Alpha, Beta, Acc, Out) :-
    playMove(Board, Move, Move, NewBoard, Player),
    changePlayer(Player, Opp),
    NewDepth is Depth - 1,
    alphabeta(NewBoard, Opp, NewDepth, Alpha, Beta, Val),
    evaluate_moves(Board, Player, Rest, Depth, Alpha, Beta, [Val-Move|Acc], Out).

alphabeta(Board, Player, 0, _, _, Val) :-
    eval_board(Board, Player, Val), !.

alphabeta(Board, Player, _, _, _, 1000) :-
    win(Board, Player), !.
alphabeta(Board, Player, _, _, _, -1000) :-
    changePlayer(Player, Opp),
    win(Board, Opp), !.

alphabeta(Board, Player, Depth, Alpha, Beta, Val) :-
    Depth > 0,
    findall(M, coup_possible(Board, M), Moves),
    Moves \= [],
    NewDepth is Depth - 1,
    best_value(Moves, Board, Player, NewDepth, Alpha, Beta, -10000, Val).

alphabeta(_, _, _, _, _, 0).  % Fall back score

best_value([], _, _, _, _, _, Val, Val).
best_value([Move|Rest], Board, Player, Depth, Alpha, Beta, BestSoFar, BestVal) :- 
    playMove(Board, Move, Move, NewBoard, Player), 
    changePlayer(Player, Opp), 
    alphabeta(NewBoard, Opp, Depth, Alpha, Beta, Val), 
    NewBest is max(BestSoFar, Val), 
    ( NewBest >= Beta -> BestVal = NewBest ; 
        max(Alpha, NewBest, NewAlpha), 
        best_value(Rest, Board, Player, Depth, NewAlpha, Beta, NewBest, BestVal) 
    ).

max(A, B, A) :- A >= B, !.
max(_, B, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIN IA — MINIMAX AVEC ALPHA-BETA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%