:-dynamic(pos/3).
:-dynamic(job/1).
:-dynamic(ekspi/1).
:-dynamic(gold/1).
:- dynamic(notInBattle/1).

:- dynamic(questFunc/3).
:- dynamic(questCounter/1).

:- dynamic(currentHP/1).

:- dynamic(equipedAcc/4).
:- dynamic(equipedWeap/4).
:- dynamic(equipedHead/4).
:- dynamic(equipedBoots/4).
:- dynamic(equipedArmor/4).


pos(player, 1, 1).
pos(wolf, 1, 5).

job(swordman).
ekspi(999).
gold(100).

currentHP(100).
notInBattle(true).
questFunc(0,0,0).
questCounter(1).

equipedWeap('Baseball Bat', 'atk', '+', 1).
equipedArmor('Uniqlo Tshirt', 'hp', '+', 0).
equipedHead('Baseball Helmet', 'def', '+', 0).
equipedBoots('Sendal Swallow', 'hp', '+', 1). 
equipedAcc('Ali Ring', 'atk', '+', 1). 

save :-
    write('Saving Data ...'),
	open('saveData.pl', write, Stream),
	write(Stream, ':- dynamic(pos/3).\n'),

    write(Stream, ':- dynamic(job/1).\n'),
    write(Stream, ':- dynamic(ekspi/1).\n'),
    write(Stream, ':- dynamic(gold/1).\n'),
    write(Stream, ':- dynamic(notInBattle/1).\n'),

    write(Stream, ':- dynamic(questFunc/3).\n'),
    write(Stream, ':- dynamic(questCounter/1).\n'),

    write(Stream, ':- dynamic(currentHP/1).\n'),

    write(Stream, ':- dynamic(equipedAcc/4).\n'),
    write(Stream, ':- dynamic(equipedWeap/4).\n'),
    write(Stream, ':- dynamic(equipedHead/4).\n'),
    write(Stream, ':- dynamic(equipedBoots/4).\n'),
    write(Stream, ':- dynamic(equipedArmor/4).\n'),

    write(Stream, '\n\n'),

    pos(player, X, Y),
    write(Stream, 'pos(player,'),
    write(Stream, X),
    write(Stream, ','),
    write(Stream, Y),
    write(Stream, ').\n'),

    job(A),
    write(Stream, 'job('),
    write(Stream, A),
    write(Stream, ').\n'),

    ekspi(B),
    write(Stream, 'ekspi('),
    write(Stream, B),
    write(Stream, ').\n'),

    gold(C),
    write(Stream, 'gold('),
    write(Stream, C),
    write(Stream, ').\n'),

    currentHP(D),
    write(Stream, 'currentHP('),
    write(Stream, D),
    write(Stream, ').\n'),

    questCounter(E),
    write(Stream, 'questCounter('),
    write(Stream, E),
    write(Stream, ').\n'),

    notInBattle(F),
    write(Stream, 'notInBattle('),
    write(Stream, F),
    write(Stream, ').\n'),

    questFunc(X1, Y1, Z1),
    write(Stream, 'questFunc('),
    write(Stream, X1),
    write(Stream, ','),
    write(Stream, Y1),
    write(Stream, ','),
    write(Stream, Z1),
    write(Stream, ').\n'),

    equipedWeap(X2, Y2, Z2, W2),
    write(Stream, 'equipedWeap('),
    write(Stream, '\''),
    write(Stream, X2),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Y2),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Z2),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, W2),
    write(Stream, ').\n'),

    equipedArmor(X3, Y3, Z3, W3),
    write(Stream, 'equipedArmor('),
    write(Stream, '\''),
    write(Stream, X3),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Y3),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Z3),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, W3),
    write(Stream, ').\n'),

    equipedHead(X4, Y4, Z4, W4),
    write(Stream, 'equipedHead('),
    write(Stream, '\''),
    write(Stream, X4),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Y4),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Z4),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, W4),
    write(Stream, ').\n'),

    equipedBoots(X5, Y5, Z5, W5),
    write(Stream, 'equipedBoots('),
    write(Stream, '\''),
    write(Stream, X5),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Y5),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Z5),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, W5),
    write(Stream, ').\n'),

    equipedAcc(X6, Y6, Z6, W6),
    write(Stream, 'equipedAcc('),
    write(Stream, '\''),
    write(Stream, X6),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Y6),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, Z6),
    write(Stream, '\''),
    write(Stream, ','),
    write(Stream, W6),
    write(Stream, ').\n'),

 	close(Stream).

load :-
    retract(pos(player,_,_)),
    retract(job(_)),
    retract(ekspi(_)),
    retract(gold(_)),
    retract(currentHP(_)),
    retract(questCounter(_)),
    retract(notInBattle(_)),
    
    retract(questFunc(_,_,_)),

    retract(equipedWeap(_,_,_,_)),
    retract(equipedArmor(_,_,_,_)),
    retract(equipedHead(_,_,_,_)),
    retract(equipedBoots(_,_,_,_)),
    retract(equipedAcc(_,_,_,_)),

 	consult('saveData.pl'),
    write('Loading Data ...').


aa :-
    pos(player, 1, 1),
    write('Testing...').
    
