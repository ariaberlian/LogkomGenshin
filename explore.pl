:- dynamic(started/1).
:- dynamic(pos/3).
:- dynamic(notInBattle/1).
:- dynamic(enemy/1).


%% Setelah dicompile, maka user harus melakukan "star." terlebih dahulu


%% FACT
%% ==============================================================
started(false).
notInBattle(true).

enemy(null).

pos(player, 1, 1).
pos(store, 7, 2).
pos(boss, 20, 8).
pos(quest, 33, 16).

pos(portal, 5, 4).
pos(portal, 34, 13).

pos(obstacle, 19, 7).
pos(obstacle, 20, 7).
pos(obstacle, 21, 7).
pos(obstacle, 22, 7).
pos(obstacle, 23, 7).
pos(obstacle, 24, 7).
pos(obstacle, 19, 8).
pos(obstacle, 24, 8).
pos(obstacle, 19, 9).
pos(obstacle, 20, 9).
pos(obstacle, 24, 9).
pos(obstacle, 22, 10).
pos(obstacle, 23, 10).
pos(obstacle, 24, 10).


%% MAIN MENU
%% ==============================================================

%% Setelah dicompile, maka user harus melakukan "star." terlebih dahulu
start :-
	retract(pos(player, _, _)),
	retract(enemy(_)),
	retract(notInBattle(_)),
	retract(started(_)),

	asserta(pos(player, 1, 1)),
	asserta(enemy(null)),
	asserta(notInBattle(true)),
	asserta(started(true)),

	write('Game is Starting...'), nl,
	write('This is your map : '), nl,
	map,
	!.


%% MAPPING
%% ==============================================================
segment(X, Y) :-
	pos(player, X, Y),
	write('P'),
	!.
segment(X, Y) :-
	pos(quest, X, Y),
	write('Q'),
	!.
segment(X, Y) :-
	pos(store, X, Y),
	write('S'),
	!.
segment(X, Y) :-
	pos(boss, X, Y),
	write('D'),
	!.
segment(X, Y) :-
	pos(obstacle, X, Y),
	write('#'),
	!.

segment(X, Y) :-
	pos(portal, X, Y),
	write('T'),
	!.

segment(_, Y) :-
	Y = 0,
	write('#'),
	!.
segment(_, Y) :-
	Y = 18,
	write('#'),
	!.
segment(X, Y) :-
	Y > 0,
	Y < 18,
	X = 0,
	write('#'),
	!.
segment(X, Y) :-
	Y > 0,
	Y < 18,
	X = 38,
	write('#'),
	!.
segment(_, _) :-
	write('-'),
	!.


hor(39, _) :- !.
hor(X, Y) :-
	segment(X, Y),
	X1 is X + 1,
	hor(X1, Y),
	!.
ver(19) :- !.
ver(Y) :-
	hor(0, Y),
	nl,
	Y1 is Y + 1,
	ver(Y1),
	!.

map :-
	started(true),
	ver(0).



%% EXPLORING
%% ==============================================================
setNotInBattle(X) :-
	X = 1,
	notInBattle(false),
	retract(notInBattle(false)),
	asserta(notInBattle(true)),
	retract(enemy(_)),
	asserta(enemy(null)),
	!.
setNotInBattle(X) :-
	X = 0,
	notInBattle(true),
	retract(notInBattle(true)),
	asserta(notInBattle(false)),
	!.

writeArah(M) :-
	M = 0,
	write('North'),
	!.
writeArah(M) :-
	M = 1,
	write('South'),
	!.
writeArah(M) :-
	M = 2,
	write('West'),
	!.
writeArah(M) :-
	M = 3,
	write('East'),
	!.


setEnemy(X) :-
	X = 0,
	write('You found a Wolf!!!!'),
	retract(enemy(_)),
	asserta(enemy(wolf)),
	!.
setEnemy(X) :-
	X = 1,
	write('You found a Slime!!!!'),
	retract(enemy(_)),
	asserta(enemy(slime)),
	!.
setEnemy(X) :-
	X = 2,
	write('You found a Goblin!!!!'),
	retract(enemy(_)),
	asserta(enemy(goblin)),
	!.
setEnemy(X) :-
	X = 3,
	write('Is it the final battle??!!!'),
	retract(enemy(_)),
	asserta(enemy(dragon)),
	!.

foundEnemy(X, _) :-
	X < 2,
	setNotInBattle(0),
	random(0, 2, Z),
	setEnemy(Z),
	!.
foundEnemy(X, M) :-
	X > 1,
	writeArah(M),
	!.

randomEnemy(Arah) :-
	random(0, 10, Z),
	foundEnemy(Z, Arah),
	!.



boosPos(X, Y) :-
	pos(boss, X, Y),
	setEnemy(3),
	!.



notWall(X, Y) :-
	X > 0,
	X < 38,
	Y > 0,
	Y < 18,
	\+pos(obstacle, X, Y).

notPortal(X, Y) :-
	\+pos(portal, X, Y).


move(X1, Y1) :-
	pos(player, X, Y),
	retract(pos(player, X, Y)),
	asserta(pos(player, X1, Y1)).


run :-
	\+enemy(dragon),
	setNotInBattle(1),
	write('Are you afraid ?!!'),
	!.

teleport(X, Y) :-
	pos(portal, X, Y),
	X = 5,
	Y = 4,
	X1 is 34,
	Y1 is 13,
	move(X1, Y1),
	write('Hiraishin no Jutsu...'), nl,
	map,
	!.

teleport(X, Y) :-
	pos(portal, X, Y),
	X = 34,
	Y = 13,
	X1 is 5,
	Y1 is 4,
	move(X1, Y1),
	write('Hiraishin no Jutsu...'), nl,
	map,
	!.



%% Movement
%% ==============================================================

w :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X + 0,
	Y1 is Y - 1,
	notWall(X1, Y1),
	move(X1, Y1),

	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
	randomEnemy(0), !.

s :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X + 0,
	Y1 is Y + 1,
	notWall(X1, Y1),
	move(X1, Y1),

	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
	randomEnemy(1), !.

a :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X - 1,
	Y1 is Y + 0,
	notWall(X1, Y1),
	move(X1, Y1),
	
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
	randomEnemy(2), !.

d :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X + 1,
	Y1 is Y + 0,
	notWall(X1, Y1),
	move(X1, Y1),
	
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
	randomEnemy(3), !.


