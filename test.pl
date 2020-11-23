:- dynamic(job/1).
:- dynamic(jobNotFound/1).
:- dynamic(jobFound/1).
:- dynamic(wolfKillCount/1).
:- dynamic(goblinKillCount/1).
:- dynamic(slimeKillCount/1).
:- dynamic(posisiPlayer/2).
:- dynamic(potion/3).
:- dynamic(equipedPotion/2).
:- dynamic(battle/1).
:- dynamic(monster/1).
:- dynamic(monsterLevel/1).
:- dynamic(ekspi/1).
:- dynamic(gold/1).
% DYNAMIC disini adalah supaya bisa kita retract(hapus) dan assert(tambahin).


% weapon(NAMA WEAPON, JENIS, STAT, Efek).
equipedWeapon(wooden_sword, single_handed-sword, 100).

potion(rage, 10, 10).

potion(health, 20, 0).
potion(health, 20, 0).
potion(health, 20, 0).
potion(health, 20, 0).
potion(health, 20, 0).


equipedPotion(health, _).
equipedPotion(rage, 0).

totalHealthPotion(N) :- 
    findall(X,potion(health,_,X),L),
    length(L,N).
totalHealthPotion(N) :- 
    findall(X,potion(health,X),L),
    length(L,N).

nguranginPotionHp :-
    retract(potion(health, 20, 0)).

nguranginPotionRage :-
    retract(potion(health, 20)).

usePotion :-
    write('Pilih Potion:'), nl,
    write('1. Health Potion'), nl,
    write('2. Rage Potion'), nl,
    write('>> '), read(Input),
    potionInput(Input).

potionInput(1) :-
    totalHealthPotion(X),
    X > 0,
    potion(health, Y, 0),
    nguranginPotionHp,
    retract(equipedPotion(health, _)),
    asserta(equipedPotion(health, Y)).


% maksimumHP(HP) :-
%     baseSTAT(_, _, _, Hape),


% calculatedATK(ATK) :-
%     baseSTAT(BAtk, _, BInt, _);
%     equipedPotion
%     ATK is BAtk +
%     i


jobNotFound(true).
slimeKillCount(0).
wolfKillCount(0).
goblinKillCount(0).

absolute(X,Y):-
    Y is round(sqrt(X**2)).


randomMonsterLevel(MyLevel, EnemyLevel) :-
    Low is MyLevel - 3,
    High is MyLevel + 3,
    random(Low,High,Lev), % RANDOMIZE LEVEL MONSTER +- 3 dari level kita
    absolute(Lev, EnemyLevel).

enemy(goblin).

monsterSTAT(ATK, HP) :-
    monsterLevel(LVL),
    ATK is round(300*1.1**(LVL)),
    HP is round(400*1.1**(LVL)),
    enemy(goblin),!.
    
monsterSTAT(ATK, HP) :-
    monsterLevel(LVL),
    ATK is round(200*1.1**(LVL)),
    HP is round(200*1.1**(LVL)),
    enemy(slime),!.

monsterSTAT(ATK, HP) :-
    monsterLevel(LVL),
    ATK is round(400*1.1**(LVL)),
    HP is round(400*1.1**(LVL)),
    enemy(wolf),!.

% pureSTAT()


posisiPlayer(3,3).

ekspi(0).
gold(0).

xpNextLVL(X) :-
    level(Y),
    X is round((Y/2)*(2000 +(Y-1)*200)).

level(X) :-
    ekspi(Y),
    X is floor((-900 + sqrt(900**2 + 400*Y))/200) + 1. 

status:-
    job(Job),
    level(Level),
    ekspi(XP),
    xpNextLVL(XP2),
    write('Status anda:'), nl,
    write('Job anda: '), write(Job), nl,
    write('Level: '), write(Level), nl,
    write('Exp: '), write(XP),write('/'),write(XP2), nl.


% (ANGKA DIBAWAH SUDAH SESUAI DOCS)
% Jadi ceritanya ATK/DEF/INT/HP itu stat awal yg keubah hanya dengan level
% ntr ada stat lain kyk ATK doang itu jadi buat multipler base atk kita sama misal equipment

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(swordsman),
    ATK is 100*(1.1**(LVL-1)),
    DEF is 350*(1.1**(LVL-1)),
    INT is 50*(1.1**(LVL-1)),
    HP is 2000*(1.1**(LVL-1)),!.
    

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(archer),
    ATK is 150*(1.1**(LVL-1)),
    DEF is 100*(1.1**(LVL-1)),
    INT is 50*(1.1**(LVL-1)),
    HP is 1500*(1.1**(LVL-1)),!.

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(wizard),
    ATK is 50*(1.1**(LVL-1)),
    DEF is 120*(1.1**(LVL-1)),
    INT is 150*(1.1**(LVL-1)),
    HP is 1700*(1.1**(LVL-1)),!.

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(priest),
    ATK is 80*(1.1**(LVL-1)),
    DEF is 150*(1.1**(LVL-1)),
    INT is 150*(1.1**(LVL-1)),
    HP is 2200*(1.1**(LVL-1)),!.

baseSTAT(100, 100, 170, 2000) :-
    job(programmer),!.

baseSTAT(9999, 9999, 9999, 999999) :-
    job(god).
    




% hey jangan makan kerjaan orang wkwkwkw

fungsi :-
    jobFound(true),
    write('Anda sudah memilih character'),!.


ascend :-
    jobFound(true), !, write('Anda sudah memilih Job'), fail;
    % level(LVL),
    % LVL > 2,
    write('Youkouso'), nl,
    write('  sSSSs   d sss   d s  b   sss. d    d d d s  b        sss. d sss   d     S d s.   d'), nl,
    write(' S     S  S       S  S S d      S    S S S  S S      d      S       S    P  S  ~O  S'), nl,
    write('S         S       S   SS Y      S    S S S   SS      Y      S       Ssss\'   S   `b S'), nl,
    write('S         S sSSs  S    S   ss.  S sSSS S S    S        ss.  S sSSs  S   s   S sSSO S'), nl,
    write('S    ssSb S       S    S      b S    S S S    S           b S       S    b  S    O S'), nl,
    write(' S     S  S       S    S      P S    S S S    S           P S       S     b S    O S'), nl,
    write('  "sss"   P sSSss P    P ` ss\'  P    P P P    P      ` ss\'  P sSSss P     P P    P P'), nl,
    write('Choose your character:'), nl,
    write('1. Swordsman'), nl,
    write('2. Archer'), nl,
    write('3. Wizard'), nl,
    write('4. Priest'), nl,
    write('>> '),
    read(Character),
    pengecekan(Character).
    
pengecekan(1) :-
    write('Anda menjadi swordsman'),
	asserta(job(swordsman)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(2) :-
    write('Anda menjadi archer'),
    asserta(job(archer)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(3) :-
    write('Anda menjadi wizard'),
	asserta(job(wizard)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(4) :-
    write('Anda menjadi priest'),
	asserta(job(priest)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(5) :-
    write('!!! ALLERT !!! ANDA MENJADI CARRY TUBES HATI-HATI'),
	asserta(job(god)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(X) :-
    X > 5,
    write('Tidak ada job tersebut'),!.

pos :-
    posisiPlayer(X,Y),
    write('Anda di koordinat:'), nl,
    write('X: '), write(X), nl,
    write('Y: '), write(Y), nl.

w :-
    \+ battle(found),
    random(1,5,Seed),
    cekJalanW(Seed).


a :-
    \+ battle(found),
    random(1,5,Seed),
    cekJalanA(Seed).

s :-
    \+ battle(found),
    random(1,5,Seed),
    cekJalanS(Seed).

d :-
    \+ battle(found),
    random(1,5,Seed),
    cekJalanD(Seed).

% JALAN W
cekJalanW(Seed) :-
    Seed > 1,
    posisiPlayer(X,Y),
    Y \= 1,
    write('Anda bergerak ke utara'),
    retractall(posisiPlayer(_,_)),
    Z is Y-1,
    asserta(posisiPlayer(X,Z)),!.
cekJalanW(Seed) :-
    Seed =:= 1,
    asserta(battle(found)),
    random(1, 10, Y),
    monsterType(Y, Z),
    write('Anda bertemu '), write(Z), nl,
    write('Apa yang akan anda lakukan?'), nl.

% JALAN A
cekJalanA(Seed) :-
    Seed > 1,
    posisiPlayer(X,Y),
    X \= 1,
    write('Anda bergerak ke barat'),
    retractall(posisiPlayer(_,_)),
    Z is X-1,
    asserta(posisiPlayer(Z,Y)),!.
cekJalanA(Seed) :-
    Seed =:= 1,
    asserta(battle(found)),
    random(1, 10, Y),
    monsterType(Y, Z),
    write('Anda bertemu '), write(Z), nl,
    write('Apa yang akan anda lakukan?'), nl.

% JALAN S
cekJalanS(Seed) :-
    Seed > 1,
    posisiPlayer(X,Y),
    Y \= 20,
    retractall(posisiPlayer(_,_)),
    write('Anda bergerak ke selatan'),!,
    Z is Y+1,
    asserta(posisiPlayer(X,Z)),!.
cekJalanS(Seed) :-
    Seed =:= 1,
    asserta(battle(found)),
    random(1, 10, Y),
    monsterType(Y, Z),
    write('Anda bertemu '), write(Z), nl,
    write('Apa yang akan anda lakukan?'), nl.


% JALAN D
cekJalanD(Seed) :-
    Seed > 1,
    posisiPlayer(X,Y),
    X \= 20,
    write('Anda bergerak ke timur'),
    retractall(posisiPlayer(_,_)),
    Z is X+1,
    asserta(posisiPlayer(Z,Y)),!.
cekJalanD(Seed) :-
    Seed =:= 1,
    asserta(battle(found)),
    random(1, 10, Y),
    monsterType(Y, Z),
    write('Anda bertemu '), write(Z), nl,
    write('Apa yang akan anda lakukan?'), nl.


monsterType(Y, goblin) :-
    Y > 0,
    Y < 4,
    retractall(monster(_)),
    retractall(monsterLevel(_)),
    asserta(monster(goblin)),
    level(MyLevel),
    randomMonsterLevel(MyLevel, EnemyLevel),
    asserta(monsterLevel(EnemyLevel)),!.

monsterType(Y, slime) :-
    Y > 3,
    Y < 7,
    retractall(monster(_)),
    retractall(monsterLevel(_)),
    asserta(monster(slime)),
    level(MyLevel),
    randomMonsterLevel(MyLevel, EnemyLevel),
    asserta(monsterLevel(EnemyLevel)),!.

monsterType(Y, wolf) :-
    Y > 6,
    Y < 10,
    retractall(monster(_)),
    retractall(monsterLevel(_)),
    asserta(monster(wolf)),
    level(MyLevel),
    randomMonsterLevel(MyLevel, EnemyLevel),
    asserta(monsterLevel(EnemyLevel)),!.


monsterEXPGOLD(XP, GOLD) :-
    XP is 500,
    GOLD is 900,
    monster(wolf),!.

monsterEXPGOLD(XP, GOLD) :-
    XP is 300,
    GOLD is 500,
    monster(goblin),!.

monsterEXPGOLD(XP, GOLD) :-
    XP is 200,
    GOLD is 300,
    monster(slime),!.


cekHPMusuh(X) :-
    X =< 0,
    monsterEXPGOLD(EXPGain,GoldGain),
    ekspi(CurrentEXP),
    gold(CurrentGold),
    retractall(ekspi(_)),
    retractall(gold(_)),
    TotalEXP is CurrentEXP + EXPGain,
    TotalGold is CurrentGold + GoldGain,
    asserta(ekspi(TotalEXP)),
    asserta(ekspi(TotalGold)),
    write('Anda mendapatkan '), write(EXPGain), write(' XP'), nl,
    write('Anda mendapatkan '), write(GoldGain), write(' Gold, total Gold: '), write(TotalGold), nl,
    retractall(battle(found)),!.

cekHPMusuh(X) :-
    X > 0,
    monsterSTAT(ATK, HP)),
    % monster nyerang diapain gtu


% attack :-
%     battle(found),
%     monsterSTAT(ATK, HP),
%     HP =< 0, !, 
%     monsterEXPGOLD(EXPGain,GoldGain),
%     ekspi(CurrentEXP),
%     gold(CurrentGold),
%     retractall(ekspi(_)),
%     retractall(gold(_)),
%     TotalEXP is CurrentEXP + EXPGain,
%     TotalGold is CurrentGold + GoldGain,
%     asserta(ekspi(TotalEXP)),
%     asserta(ekspi(TotalGold)),
%     write('Anda mendapatkan '), write(EXPGain), write(' XP'), nl,
%     write('Anda mendapatkan '), write(GoldGain), write(' Gold, total Gold: '), write(TotalGold), nl,
%     retractall(battle(_)),fail;
%     battle(found),
%     monsterSTAT(ATK, HP),

