- [Projet Puissance 4 ALIA](#projet-puissance-4-alia)
- [Jouer au jeu](#jouer-au-jeu)
  - [Lancer le projet](#lancer-le-projet)
  - [Choisir les adversaires](#choisir-les-adversaires)
- [Fonctionnement](#fonctionnement)
  - [Modélisation du jeu](#modélisation-du-jeu)
  - [Fichiers](#fichiers)

# Projet Puissance 4 ALIA

Projet réalisé par Rami KHEDAIR et Adrien ROHO. 
Ce README donne des informations sur comment lancer le jeu, comment nous l'avons implémenté etc...

# Jouer au jeu

Différentes informations pour savoir comment jouer au jeu et comment choisir les IA.

## Lancer le projet

Lancer `game.bat`, si vous avez swipl dans votre variable PATH.
Vous pourrez ensuite utiliser la requête `init.` pour démarrer le jeu.

## Choisir les adversaires

Il est possible de modifier le code pour utiliser nos différentes IA dans le jeu. 
Les IA disponibles 
- `human`, pas vraiment une IA, permet à quelqu'un de jouer dans la console
- `random`, IA aléatoire implémentée au tout début
- `minmax`, (Base générée par IA puis repris par nous) tentative d'algo avec alpha bêta. Ne fonctionne pas à cause de la fonction qui calcule un score pour chaque coup, qui renvoie toujours 0. 
- `A*`, (Base générée par IA puis repris par nous). Implémentation d'un algorithme A* avec attribution d'un score pour chaque coup

# Fonctionnement

Informations techniques liées à notre implémentation.

## Modélisation du jeu 

Nous sommes partis, comme la majorité des groupes, sur la base de Morpion que nous avions implémenté en cours. Nous représentons notre plateau comme étant une liste unique, en colonne:

0, 6, 12, ...
1, 7, 13, ...
2, 8, 14, ...
3, 9, 15, ...
.  .  .  
.  .  .
.  .  .

L'ensemble du jeu a été réalisé par nous même, à l'exception du prédicat d'affichage du jeu que nous avons généré par IA. 
Notre version n'a pas été mise à jour pour fonctionner avec le système de tournoi qui a été implémenté durant le projet.

## Fichiers

Le code est séparé en plusieurs fichiers. 
- `display.pl` contient la logique d'affichage du jeu.
- `game.pl` contient la logique de jeu. 
- `main.pl` orchestrateur des différents fichiers. Il contient la boucle de jeu et permet de choisir l'IA à jouer. 
- `shared.pl` contient des prédicats qui sont communs à plusieurs IA ou utilisés par le jeu par exemple.
- `<ia>.pl` contiennent le code de chacune des méthodes de jeu implémentées. Ces méthodes sont présentées dans [](#choisir-les-adversaires)
