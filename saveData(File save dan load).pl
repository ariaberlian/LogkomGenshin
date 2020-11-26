:- dynamic(pos/3).
:- dynamic(job/1).
:- dynamic(ekspi/1).
:- dynamic(gold/1).
:- dynamic(notInBattle/1).
:- dynamic(questFunc/3).
:- dynamic(questCounter/1).
:- dynamic(currentHP/1).
:- dynamic(equipedAcc/4).
:- dynamic(equipedWeap/4).
:- dynamic(equipedHead/4).
:- dynamic(equipedBoots/4).
:- dynamic(equipedArmor/4).


pos(player,1,1).
job(swordman).
ekspi(999).
gold(100).
currentHP(100).
questCounter(1).
notInBattle(true).
questFunc(0,0,0).
equipedWeap('Baseball Bat','atk','+',1).
equipedArmor('Uniqlo Tshirt','hp','+',0).
equipedHead('Baseball Helmet','def','+',0).
equipedBoots('Sendal Swallow','hp','+',1).
equipedAcc('Ali Ring','atk','+',1).
