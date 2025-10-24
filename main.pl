:- use_module(display).
:- use_module(game).
:- use_module(human_random).
:- use_module(astar).
:- use_module(minmax).

:- dynamic board/1. % permet l'assertion et le retrait de faits board/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA players
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Possibilité d'utiliser plusieurs prédicats, qui prennent tous les 3 même paramètres.
% human, pour jouer dans la console
% iaRandom, pour une IA aléatoire
% iaMinMax, pour une tentative d'algo min max avec élagage alpha bêta
% astar, pour l'impléntation de l'algo a*

ia(Board, Move, 'o') :-
    iaAStar(Board, Move, 'o').
ia(Board, Move, 'x'):-
    iaRandom(Board, Move, 'x').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JEU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

applyIt(Board, NewBoard):-
    retract(board(Board)), assert(board(NewBoard)).

play(Player):-
    board(Board),
    win(Board,Player),
    format('Victoire de ~w!~n', [Player]),
    retract(board(Board)), !.

play(Player):-
   	write('New turn for:'), writeln(Player),
    board(Board), % instanciate the board from the knowledge base
    displayBoard(Board), % print it
	ia(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
	playMove(Board,Move,Move,NewBoard,Player), % Play the move and get the result in a new Board
	applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
	changePlayer(Player,NextPlayer), % Change the player before next turn
	play(NextPlayer). % next turn!

%%%%% Start the game!
init :- length(Board,42), assert(board(Board)), (play('x')).