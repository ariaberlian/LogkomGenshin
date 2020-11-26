%%Setelah bertemu dengan musuh, pemain akan memasuki tampilan bertarung. 
%%Pada tampilan ini, pemain dapat melakukan attack, special attack, use potions, atau run. 
%%Saat melakukan attack dan special attack, pemain akan mengurangi HP musuh. 
%%Special attack dapat digunakan setiap 3 turn sekali. 
%%Use potions akan memakai potion yang terdapat pada inventory. 
%%Jika tidak terdapat potion, akan mengulang turn untuk melakukan attack, special attack, atau run. 
%%Saat melakukan run, terdapat kemungkinan run gagal, kemudian turn berpindah ke musuh.
%%Saat turn musuh, musuh dapat melakukan special attack atau attack. Sistem turn musuh dibebaskan implementasinya.


:- dynamic(win/1).
:- dynamic(lose/1).
:- dynamic(hp/2).
:- dynamic(notInBattle/1)
:- dynamic(turn/1)
:- dynamic(cdSkill/1)
:- dynamic(cdBurst/1)
:- dynamic(cdRage/1)
:- dynamic(cdSmart/1)
:- dynamic(cdRock/1)

win(false).
lose(false).

notInBattle(false).

turn(0).

cdSkill(0).
cdBurst(0).

cdRage(0).
cdSmart(0).
cdRock(0).

battlewith(Monster).

battle :-
	
	battlewith(Monster),	
	\+hp(0, player),
	\+hp(0, Monster),
	write ('Your HP is'),
	write (HP(_, player)), nl,
	write ('The '),
	write (monster(_,Monster)),
	write (''s HP is'),
	write (HP(_, Monster)), nl,
	write ('What would you do?'), nl,
	write ('1. Normal Attack'), nl,
	write ('2. Skill'), nl,
	write ('3. Burst Attack'), nl,
	write ('4. Use Potion'), nl,
	write ('5. RUN !!!'), nl,
	write ('>> '),
	read (Opsi),
	action(Opsi),
	turn(N),
	N1 is N + 1,
	cdSkill(A),
	cdBurst(B),
	cdSpell,
	retract(turn(_)),
	assert(turn(N1)).

%%APABILA SPELL HABIS, MASIH CD ,DLL. JUMLAH TURN TIDAK DITAMBAHKAN

rebattle :-

	\+hp(0, player),
	\+hp(0, Monster),
	write ('Your HP is'),
	write (HP(_, player)), nl,
	write ('The '),
	write (monster(_,Monster)),
	write (''s HP is'),
	write (HP(_, Monster)), nl,
	write ('What would you do?'), nl,
	write ('1. Normal Attack'), nl,
	write ('2. Skill'), nl,
	write ('3. Burst Attack'), nl,
	write ('4. Use Potion'), nl,
	write ('5. RUN !!!'), nl,
	write ('>> '),
	read (Opsi),
	action(Opsi).

%% SISTEM COOLDOWN

cdSkill(A) :-

	A > 0,
	A1 is A -1,
	retract(cdSkill(_)),
	assert(cdSkill(A1)).


cdBurst(B) :-
	B > 0,
	B1 is B -1,
	retract(cdBurst(_)),
	assert(cdBurst(B1)).


cdSpell :-

	cdRage(C),
	C > 0,
	C1 is C -1,
	retract(cdRage(_)),
	assert(cdRage(C1)).
	
cdSpell :-

	cdSmart(D),
	D > 0,
	D1 is D -1,
	retract(cdSmart(_)),
	assert(cdSmart(D1)).

cdSpell :-

	cdRock(E),
	E > 0,
	E1 is E -1,
	retract(cdRock(_)),
	assert(cdRock(E1)).


action(1) :- 

	notInBattle(false),
	ATK(X, player),
	HP(Y,monster(M)),
	A is Y-X,
	retract(HP(Y,monster(M))),
	assert(HP(A, monster(M))),
	counterATK(Z).

action(2) :- 

	cdSkill(0),
	notInBattle(false),
	SkillATK(X, player),
	HP(Y,monster(M)),
	A is Y-X,
	retract(HP(Y,monster(M))),
	assert(HP(A, monster(M))),
	retract(cdSkill(_)),
	assert(cdSkill(2)),
	counterATK.

action(2) :- 

	cdSkill(X),
	X > 0,
	write('Your Skill still in cooldown'),
	rebattle.

counterATK :-

	turn(Z),
	\+(Z%3 is 0),
	ATK(X, monster(M)),
	HP(Y, player),
	def(Y1, player),
	A is Y-X+Y1,
	retract(HP(Y, player)),
	assert(HP(A, player)),
	battle.

counterATK :-

	turn(Z),
	Z%3 is 0,
	SPATK(X, monster(M)),
	HP(Y, player),
	def(Y1, player),
	A is Y-X+Y1,
	A is Y-X,
	retract(HP(Y, player)),
	assert(HP(A, player)),
	battle.

action(3) :- 

	cdSkill(0),
	notInBattle(false),
	burstATK(X, player),
	HP(Y,monster(M)),
	A is Y-X,
	retract(HP(Y,monster(M))),
	assert(HP(A, monster(M))),
	retract(cdBurst(_)),
	assert(cdBurst(3)),
	counterATK.

action(3) :- 

	cdSkill(X),
	X > 0,
	write('Your Burst Skill still in cooldown'),
	rebattle.

action(4) :-
	write ('Which potion would you use?'), nl,
	write ('1. Health Potion (S)'), nl,
	write ('2. Health Potion(M)'), nl,
	write ('3. Health Potion(L)'), nl,
	write ('4. Rage Potion'), nl,
	write ('5. Smart Potion'), nl,
	write ('6. Rock Potion'), nl,
	write ('>>'),nl,
	read (Potion),
	potion(Potion).
	

action(5) :- 
	random(1, 2, N),
	run(N, X).

run(1, X) :-
	write ('Selamat, Anda berhasil kabur dari monster'), write (X), nl,
	write ('Kamu dapat '), write (rewardexp), write (' EXP dan '), write (rewardgold), write (' gold').
	
run(2, X) :-
	write ('Sayangnya, kamu gagal.'), nl,
	counterATK.

potion(1) :-
	
	potion(X, health+15),
	X > 1,
	X1 is X - 1,
	hp(Y, player),
	Y1 is Y + maxHP*1.15
	retract(HP(Y, player)),
	assert(HP(Y1, player)),
	counterATK.

potion(1) :-
	
	potion(0, health+15),
	write ('Health Potion (S) sudah habis.. :(')
	battle.


potion(2) :-
	
	potion(X, health+25),
	X > 1,
	X1 is X - 1,
	hp(Y, player),
	Y1 is Y + maxHP*1.25,
	retract(HP(Y, player)),
	assert(HP(Y1, player)),
	counterATK.

potion(2) :-
	
	potion(0, health+25),
	write ('Health Potion (M) sudah habis.. :('),
	battle.

potion(3) :-
	
	potion(X, health+50),
	X > 1,
	X1 is X - 1,
	hp(Y, player),
	Y1 is Y + maxHP*1.50,
	retract(HP(Y, player)),
	assert(HP(Y1, player)),
	counterATK.

potion(3) :-
	
	potion(0, health+50),
	write ('Health Potion (L) sudah habis.. :('),
	rebattle.

potion(4) :-

	cdRage(0),
	potion(X, rage),
	X > 1,
	X1 is X - 1,
	atk(Y, player),
	Y1 is Y*1.10,
	retract(atk(Y, player)),
	assert(atk(A, player)),
	counterATK.

potion(4) :-

	cdRage(N),
	N > 0,
	potion(X, rage),
	X > 1,
	write ('Sudah menggunakan Rage Spell, Kamu dapat menggunakannya lagi pada '), write (N), write (' ronde berikutnya'),
	rebattle.

potion(4) :-
	
	potion(0, rage),
	write ('Rage Potion sudah habis.. :('),
	rebattle.	

potion(5) :-

	cdSmart(0),
	potion(X, smart),
	X > 1,
	X1 is X - 1,
	int(Y, player),
	Y1 is Y*1.10
	retract(int(Y, player)),
	assert(int(A, player)),
	counterATK.

potion(5) :-

	cdSmart(N),
	N > 0,
	potion(X, smart),
	X > 1,
	write ('Sudah menggunakan Smart Spell, Kamu dapat menggunakannya lagi pada '), write (N), write (' ronde berikutnya'),
	rebattle.

potion(5) :-
	
	potion(0, smart),
	write ('Smart Potion sudah habis.. :('),
	rebattle.	

potion(6) :-

	cdRock(0),
	potion(X, rock),
	X > 1,
	X1 is X - 1,
	def(Y, player),
	Y1 is Y*1.10
	retract(def(_, player)),
	assert(def(A, player)),
	counterATK.

potion(6) :-

	cdRock(N),
	N > 0,
	potion(X, rock),
	X > 1,
	write ('Sudah menggunakan Rage Spell, Kamu dapat menggunakannya lagi pada '), write (N), write (' ronde berikutnya'),
	rebattle.

potion(6) :-
	
	potion(0, rock),
	write ('Rock Potion sudah habis.. :('),
	rebattle.	


%%KETIKA SALAH SATU PIHAK HABIS HPNYA

battle(X) :- 
	HP(0,monster(_)),
	retract(win(false)),
	assert(win(true)),
	getreward,
	resetHPMonster.

battle(X) :- 
	HP(0, player),
	retract(lose(false)),
	assert(lose(true)),
	resetHPMonster.

resetHPMonster :-

	battlewith(Monster),
	retract(hp(_, Monster)),
	monsterSTAT(_,HP),
	assert(hp(HP, Monster)).	

getreward :- 
	
	battlewith(slime),
	level(Z,slime)
	exp(X, player),
	X1 = X + 100*Z
	retract(exp(_,player)),
	assert(exp(X1,player)),
	gold(Y, player),
	Y1 = Y + 1000*Z
	retract(gold(_,player)),
	assert(gold(Y1,player)).
	
getreward :- 
	battlewith(goblin),
	level(Z,goblin)
	exp(X, goblin),
	X1 = X + 150*Z
	retract(exp(_,player)),
	assert(exp(X1,player)),
	gold(Y, player),
	Y1 = Y + 1500*Z
	retract(gold(_,player)),
	assert(gold(Y1,player)).

getreward :- 
	battlewith(wolf),
	exp(X, player),
	level(Z,wolf)
	X1 = X + 200*Z
	retract(exp(_,player)),
	assert(exp(X1,player)),
	gold(Y, player),
	Y1 = Y + 2000*Z
	retract(gold(_,player)),
	assert(gold(Y1,player)).
