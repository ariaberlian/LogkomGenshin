/*Setelah bertemu dengan musuh, pemain akan memasuki tampilan bertarung. 
Pada tampilan ini, pemain dapat melakukan attack, special attack, use potions, atau run. 
Saat melakukan attack dan special attack, pemain akan mengurangi HP musuh. 
Special attack dapat digunakan setiap 3 turn sekali. 
Use potions akan memakai potion yang terdapat pada inventory. 
Jika tidak terdapat potion, akan mengulang turn untuk melakukan attack, special attack, atau run. 
Saat melakukan run, terdapat kemungkinan run gagal, kemudian turn berpindah ke musuh.
Saat turn musuh, musuh dapat melakukan special attack atau attack. Sistem turn musuh dibebaskan implementasinya.*/

:- dynamic(win/1).
:- dynamic(HP/2).
:- dynamic(notInBattle/1)

win(false).
lose(false).
notInBattle(false).

battle(X) :-
            \+HP(0, player),
            \+HP(0,monster(X)),
            write ('Your HP is'),
            write (HP(_, player)), nl,
            write ('The '),
            write (monster(X)),
            write (''s HP is'),
            write (HP(_, monster(X))), nl,
            write ('What would you do?'), nl,
            write ('1. Normal Attack'), nl,
            write ('2. Special A'), nl,
            write ('3. Use Potion'), nl,
            write ('4. RUN !!!'), nl,
            write ('>> '),
            read (Opsi),
            action(Opsi).


battle(X) :- 
            HP(0,monster(X)).
            retract(win(false)),
            assert(win(true)),	
battle(X) :- 
            HP(0, player).
            retract(lose(false)),
            assert(lose(true)),

action(1) :- 
            notInBattle(false).
            ATK(X, player),
            HP(Y,monster(M)),
            A is Y-X,
            retract(HP(Y,monster(M))),
            assert(HP(A, monster(M))),
            counterATK(Z).

action(2) :- 
            notInBattle(false).
            SPATK(X, player),
            HP(Y,monster(M)),
            A is Y-X,
            retract(HP(Y,monster(M))),
            assert(HP(A, monster(M))),
            counterATK(Z).

counterATK(Z) :-
                \+(Z%3 is 0),
                ATK(X, monster(M)),
                HP(Y, player),
                A is Y-X,
                retract(HP(Y, player)),
                assert(HP(A, player)),
                battle.

counterATK(Z) :-
                Z%3 is 0,
                SPATK(X, monster(M)),
                HP(Y, player),
                A is Y-X,
                retract(HP(Y, player)),
                assert(HP(A, player)),
                battle.

action(3) :-
            write ('Which potion would you use?'), nl,
            write ('1. Health Potion'), nl,
            write ('2. Buff Potion'), nl,	

action(4) :- 
	

potion(1) :-
            potion(X, health),
            X > 1,
            X1 is X - 1,
            retract(HP(Y, player)),
            assert(HP(A, player)),
            counterATK.

potion(2) :-
            potion(X, buff),
            X > 1,
            X1 is X - 1,
            retract(HP(Y, player)),
            assert(HP(A, player)),
            counterATK.
