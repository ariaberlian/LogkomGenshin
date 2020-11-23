:- dynamic(started/1).
:- dynamic(pos/3).
:- dynamic(notInBattle/1).
:- dynamic(enemy/1).
:- dynamic(bag/1).

:- dynamic(enemy/1).
:- dynamic(enemyLevel/1).

:- dynamic(ekspi/1).
:- dynamic(gold/1).
:- dynamic(job/1).
:- dynamic(jobFound/1).



%% FACT
%% ==============================================================
started(false).
notInBattle(true).

enemy(null).


job(programmer).
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

%  fact Equipment
% weapon(name, type, stat, statType, numberStat)
weapon('Wooden Sword', 'Single-Handed Sword', 'atk', '+', 40).
weapon('Silver Sword', 'Single-Handed Sword', 'atk', '+', 200).
weapon('Excalibur', 'Single-Handed Sword', 'atk', '%', 20).
weapon('Demon Hunter Sword', 'Single-Handed Sword', 'atk', '%', 50).
weapon('Durandal', 'Greatsword', 'atk', '+', 30).
weapon('Wolfs Gravestone', 'Greatsword', 'atk', '+', 220).
weapon('Surtr', 'Greatsword', 'atk', '%', 25).
weapon('Mistilteinn', 'Greatsword','atk', '+', 3000).
weapon('Ketapel', 'Bow', 'atk', '+', 40).
weapon('Pasopatis Bow', 'Bow', 'atk', '+', 200).
weapon('Artemis Bow', 'Bow', 'atk', '%', 20).
weapon('Dream of Last Memoire', 'Bow', 'atk', '%', 60).
weapon('Dessert Eagle', 'Crossbow','atk', '+', 70).
weapon('Windtalker', 'Crossbow', 'atk', '+', 250).
weapon('Guizhong Ballista', 'Crossbow', 'atk', '%', 30).
weapon('Dragon Slayer', 'Crossbow', 'atk', '+', 5000).
weapon('Lost Prayer', 'Book', 'int', '+', 30).
weapon('Talking Grimoire', 'Book', 'int','+', 200).
weapon('Enchanted Talisman', 'Book', 'int','%', 30).
weapon('Codex of Forgotten God', 'Book','int','%', 60).
weapon('Broom', 'Staff', 'int', '+',40).
weapon('Harry Potters Magic Wand', 'Staff', 'int', '%', 25).
weapon('Merlin', 'Staff', 'int', '+', 3000).
weapon('Reforged Sandalwood Warden Staff', 'Staff', 'int', '+', 5500).
weapon('Baseball Bat', 'None', 'atk', '+', 1).

% armor(name, stat, statType, statNum)
armor('Wooden Armor', 'hp', '+', 100).
armor('Silver Armor', 'hp', '%', 10).
armor('Golden Armor', 'hp', '%', 15).
armor('Divine Protection', 'hp', '%', 30).
armor('Uniqlo Tshirt', 'hp', '+', 0).

% head(name, stat, statType, statNum)
head('Kreuzeck Bag Helmet', 'def', '+', 30).
head('Cursed Helmet', 'def', '%', 20).
head('Sky Guardian Helmet', 'def', '+', 200).
head('Honda SNI Helmet', 'def', '%', 50).
head('Baseball Helmet', 'def', '+', 0).

% boots(name, stat, statType, statNum).
boots('Wooden Bakiak', 'hp', '+', 100).
boots('Lightning Vans', 'hp', '%', 5).
boots('Adidas x Nike', 'hp', '+', 800).
boots('Holly Swallow', 'hp', '%', 10).
boots('Sendal Swallow', 'hp', '+', 1).

% accessory(name, stat, statType, statNum).
accessory('Cassava Leaves Necklace', 'def', '%', 5).
accessory('Mermaid Mans Ring', 'atk', '+', 10).
accessory('Onee-chan Poster', 'int', '+', 25).
accessory('Fiesta SpicyChikenWings', 'int', '%', 30).
accessory('Fiesta SpicyChikenWings', 'atk', '%', 30).
accessory('Ali Ring', 'atk', '+', 1).

% fact potion
% potion(name, stat, statType, statNum)
potion('Health Potion Small', 'recover Hp', '%', 15).
potion('Health Potion Medium', 'recover Hp', '%', 25).
potion('Health Potion Large', 'recover Hp', '%', 50).
potion('Rage Potion', 'atk', '%', 10).
potion('Smart Potion', 'int', '%', 10).
potion('Rock Potion', 'def', '%', 10).

% fact equipment grade
% grade(grade, name, number)
grade('B', 'Wooden Sword', 1).
grade('B', 'Durandal', 2).
grade('B', 'Ketapel', 3).
grade('B', 'Dessert Eagle', 4).
grade('B', 'Lost Prayer', 5).
grade('B', 'Broom', 6).
grade('B', 'Wooden Armor', 7).
grade('B', 'Kreuzeck Bag Helmet', 8).
grade('B', 'Wooden Bakiak', 9).
grade('B', 'Cassava Leaves Necklace', 10).

grade('A', 'Silver Sword', 1).
grade('A', 'Wolfs Gravestone', 2).
grade('A', 'Pasopatis Bow', 3).
grade('A', 'Windtalker', 4).
grade('A', 'Talking Grimoire', 5).
grade('A', 'Harry Potters Magic Wand', 6).
grade('A', 'Silver Armor', 7).
grade('A', 'Cursed Helmet', 8).
grade('A', 'Lightning Vans', 9).   
grade('A', 'Mermaid Mans Ring', 10).

grade('S', 'Excalibur', 1).
grade('S', 'Surtr', 2).
grade('S', 'Artemis Bow', 3).
grade('S', 'Guizhong Ballista', 4).
grade('S', 'Enchanted Talisman', 5).
grade('S', 'Merlin', 6).
grade('S', 'Golden Armor', 7).
grade('S', 'Sky Guardian Helmet', 8).
grade('S', 'Adidas x Nike', 9).
grade('S', 'Onee-chan Poster', 10).

grade('SS', 'Demon Hunter Sword', 1).
grade('SS', 'Mistilteinn', 2).
grade('SS', 'Dream of Last Memoire', 3).
grade('SS', 'Dragon Slayer', 4).
grade('SS', 'Codex of Forgotten God', 5).
grade('SS', 'Reforged ', 6).
grade('SS', 'Divine Protection', 7).
grade('SS', 'Honda SNI Helmet', 8).
grade('SS', 'Holly Swallow', 9).
grade('SS', 'Fiesta SpicyChikenWings', 10).

% Bag and Wallet
bag(['Baseball Bat', 'Uniqlo Tshirt', 'Baseball Helmet', 'Sendal Swallow', 'Ali Ring','Health Potion Small','Health Potion Small','Health Potion Small','Health Potion Small','Health Potion Small']).
% gold(300).


%% MAIN MENU
%% ==============================================================

%% Setelah dicompile, maka user harus melakukan "start." terlebih dahulu
start :-
	retract(pos(player, _, _)),
	retract(enemy(_)),
	retract(notInBattle(_)),
	retract(started(_)),

    retractall(ekspi(_)),
    retractall(gold(_)),
    asserta(ekspi(3600)),
    asserta(gold(300)),
	asserta(pos(player, 1, 1)),
	asserta(enemy(null)),
	asserta(notInBattle(true)),
	asserta(started(true)),

	write('Game is Starting...'), nl,
    write('Youkouso'), nl,
    write('  sSSSs   d sss   d s  b   sss. d    d d d s  b        sss. d sss   d     S d s.   d'), nl,
    write(' S     S  S       S  S S d      S    S S S  S S      d      S       S    P  S  ~O  S'), nl,
    write('S         S       S   SS Y      S    S S S   SS      Y      S       Ssss\'   S   `b S'), nl,
    write('S         S sSSs  S    S   ss.  S sSSS S S    S        ss.  S sSSs  S   s   S sSSO S'), nl,
    write('S    ssSb S       S    S      b S    S S S    S           b S       S    b  S    O S'), nl,
    write(' S     S  S       S    S      P S    S S S    S           P S       S     b S    O S'), nl,
    write('  "sss"   P sSSss P    P ` ss\'  P    P P P    P      ` ss\'  P sSSss P     P P    P P'), nl, nl,

    write('Pada suatu hari yang cerah di negara Belzard, hiduplah seorang pemuda biasa yang sedang asik bermain baseball bersama temannya. *Ctankk* “HOMERUNNNN!!!”. Seorang anak teriak. Lalu pemuda bernama Issei berlari mengejar bola itu meskipun sudah keluar dari arena bermain. Dia terus berlari hingga berpapasan dengan truck-kun. Issei membuka mata. Dia melihat sesosok wanita cantik di depannya. Wanita itu berkata, “Sayang sekali, namun kau sudah mati. Sekarang kau punya dua pilihan, yaitu untuk pergi ke surga atau menjalani hidup baru di dunia yang baru.”. Tanpa pikir panjang, Issei menjawab, “Aku akan menjadi raja harem di dunia yang baru itu!”.'),nl,nl,

    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
    write('%                              Genshin Asik                                    %'), nl,
    write('% 1. start  : untuk memulai petualanganmu                                      %'), nl,
    write('% 2. map    : untuk menampilkan map                                            %'), nl,
    write('% 3. status : menampilkan kondisimu terkini                                    %'), nl,
    write('% 4. w      : gerak ke utara 1 langkah                                         %'), nl,
    write('% 5. s      : gerak ke selatan 1 langkah                                       %'), nl,
    write('% 6. d      : gerak ke ke timur 1 langkah                                      %'), nl,
    write('% 7. a      : gerak ke barat 1 langkah                                         %'), nl,
    write('% 9. Status : menampilkan status pemain                                        %'), nl,
    write('% 8. help   : menampilkan segala bantuan                                       %'), nl,
    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,

	write('This is isekai map : '), nl,
	map,
	!.

% ASCEND JOB
fungsi :-
    jobFound(true),
    write('Anda sudah memilih character'),!.

ascend :-
    jobFound(true), !, write('Anda sudah Ascencion'), nl, fail;
    level(LVL),
    LVL > 2,
    write('Choose your ascencion job:'), nl,
    write('1. Swordsman'), nl,
    write('2. Archer'), nl,
    write('3. Wizard'), nl,
    write('4. Priest'), nl,
    write('>> '),
    read(Character),
    pengecekan(Character).
    
pengecekan(1) :-
    write('Anda menjadi swordsman'),
    retractall(job(_)),
	asserta(job(swordsman)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(2) :-
    write('Anda menjadi archer'),
    retractall(job(_)),
    asserta(job(archer)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(3) :-
    write('Anda menjadi wizard'),
    retractall(job(_)),
	asserta(job(wizard)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(4) :-
    write('Anda menjadi priest'),
    retractall(job(_)),
	asserta(job(priest)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(5) :-
    write('!!! ALLERT !!! ANDA MENJADI CARRY TUBES HATI-HATI'),
    retractall(job(_)),
	asserta(job(god)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)),!.
pengecekan(X) :-
    X > 5,
    write('Tidak ada job tersebut'),!.




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

%% INVENTORY
%% ==============================================================
count(_, [], 0).
count(X, [H|T], Result) :- X \= H, count(X, T, Result).
count(X, [H|T], Result) :- X = H, count(X, T, Res1), Result is Res1 + 1.

dupRem([],[]).
dupRem([H|T],R) :- member(H,T), dupRem(T, R).
dupRem([H|T], [H|R]) :- dupRem(T,R).

throw(A, [A|B], B).
throw(A, [B, C|D], [B|E]) :-
    throw(A, [C|D], E).

detail(Name, Jenis, Stat, StatType, StatNum) :- weapon(Name, Jenis, Stat, StatType, StatNum).
detail(Name, 'armor', Stat, StatType, StatNum) :- armor(Name, Stat, StatType, StatNum).
detail(Name, 'head', Stat, StatType, StatNum) :- head(Name, Stat, StatType, StatNum).
detail(Name, 'boots', Stat, StatType, StatNum) :- boots(Name, Stat, StatType, StatNum).
detail(Name, 'accessory', Stat, StatType, StatNum) :- accessory(Name, Stat, StatType, StatNum).
detail(Name, 'potion', Stat, StatType, StatNum) :- potion(Name, Stat, StatType, StatNum).

% readBag(LBag, LBagUnique)
readBag(_L1, []).
readBag(L1, [X|Xs]) :- 
    count(X, L1, N),
    detail(X, Jenis, Stat, StatType, StatNum),
    write(N), write(' '),
    write(X), write(' Tipe: '),write(Jenis),write(' Efek: '),write(StatType),write(StatNum),write(' '),write(Stat),nl,
    readBag(L1, Xs).

inventory :- 
    gold(M),
    write('Jumlah Gold Anda: '),write(M),nl,
    write('Isi tas anda adalah: '),nl,
    bag(B),
    dupRem(B,C),
    readBag(B,C),!,
    write('Apakah Anda ingin melakukan sesuatu?'),nl,
    write('1. Buang Item'),nl,
    write('2. Gunakan Item'),nl,
    write('3. Keluar'),nl,
    write('>> '),
    read(Input),
    inventOption(Input),!.

inventOption(1) :- 
    write('Apa nama item yang ingin anda buang?'),nl,
    bag(B),
    % dupRem(B,C),
    % readBag(B,C),!,
    write('>>'),
    read(Input),
    (
       \+ member(Input, B)-> write('Item yang anda masukan tidak ada dalam inventory.'); write('Anda membuang '), write(Input),nl,throw(Input, B, Res),retract(bag(_)),asserta(bag(Res))
    ).

inventOption(2) :-
    write('Apa nama item yang ingin anda gunakan?'),nl,
    bag(B),
    % dupRem(B,C),
    % readBag(B,C),!,
    write('>>'),
    read(Input),
    % ( \+ member(Input, B) -> write('Item yang anda masukan tidak ada dalam inventory.'); )
    % detail(Input, Jenis, Stat, StatType, StatNum),
    % (Jenis = potion -> usepotion;useEquip),
    write('Item '), write(Input), write(' telah digunakan!'),nl.

inventOption(3):- !,fail.

%% STORE
%% ==============================================================
jajan(M, P, Result) :-
    M >= P, Result is M-P.
jajan(M, P, _Result) :-
    M < P, write('Gold anda tidak mencukupi!'),nl.

store :-
    write('Selamat datang di store. Ingin belanja apa?'),nl,
    write('1. Pandora Box 300G'),nl,
    write('2. Potion'),nl,
    write('3. Keluar'),nl,
    write('Masukkan nomor yang dipilih'),nl,
    write('>>'),
    read(Input),
    storeOpt(Input).

storeOpt(1) :-
    write('Anda telah membeli Pandora Box.'),nl,
    gold(M),
    jajan(M, 300, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Membuka Pandora Box....'),nl,
    write('Anda mendapatkan: '),gacha(Eq, Grade),write(Eq),write(' dengan tingkat kelangkaan '),write(Grade),
    bag(Y),
    push(Eq, Y, B),
    retract(bag(_)),
    asserta(bag(B)).

storeOpt(2) :-
    write('1. Health Potion Small  50G'),nl,
    write('2. Health Potion Medium 100G'),nl,
    write('3. Health Potion Large  200G'),nl,
    write('4. Rage Potion 200G'),nl,
    write('5. Smart Potion 200G'),nl,
    write('6. Rock Potion 200G'),nl,
    read(Input),
    buyPot(Input).

gacha(Eq,Grade) :-
    random(1, 101, N),
    random(1, 11, N1),
    (1 =< N,N < 66 -> grade('B', Eq, N1), Grade is 'B';true),
    (66 =< N,N < 87 -> grade('A', Eq, N1), Grade is 'A';true),
    (87 =< N,N < 96 -> grade('S', Eq, N1), Grade is 'S';true),
    (96 =< N,N < 100 -> grade('SS', Eq, N1), Grade is 'SS';true).

buyPot(1) :- 
    write('Anda telah membeli Health Potion Small.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Small', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 50, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).

buyPot(2) :- 
    write('Anda telah membeli Health Potion Medium.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Medium', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 100, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).

buyPot(3) :- 
    write('Anda telah membeli Health Potion Large.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Large', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).

buyPot(4) :- 
    write('Anda telah membeli Rage Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Rage Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).

buyPot(5) :- 
    write('Anda telah membeli Smart Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Smart Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).

buyPot(6) :- 
    write('Anda telah membeli Rock Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Rock Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)).
 


% randomMonsterLevel(MyLevel, EnemyLevel) :-
%     Low is MyLevel - 3,
%     High is MyLevel + 3,
%     random(Low,High,Lev), % RANDOMIZE LEVEL MONSTER +- 3 dari level kita
%     absolute(Lev, EnemyLevel).




%% EXPLORING
%% ==============================================================
%% TRUE
setNotInBattle(X) :-
	X = 1,
	notInBattle(false),
	retract(notInBattle(false)),
	asserta(notInBattle(true)),
	retract(enemy(_)),
	asserta(enemy(null)),
	!.
%% FALSE
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

%% WOLF
setEnemy(X) :-
	X = 0,
	write('You found a Wolf!!!!'),
	retract(enemy(_)),
	asserta(enemy(wolf)),
	
	!.
%% SLIME
setEnemy(X) :-
	X = 1,
	write('You found a Slime!!!!'),
	retract(enemy(_)),
	asserta(enemy(slime)),
	!.
%% GOBLIN
setEnemy(X) :-
	X = 2,
	write('You found a Goblin!!!!'),
	retract(enemy(_)),
	asserta(enemy(goblin)),
	!.
%% DRAGON
setEnemy(X) :-
	X = 3,
	write('Is it the final battle??!!!'),
	retract(enemy(_)),
	asserta(enemy(dragon)),
	!.

%% MENEMUKAN ENEMY
foundEnemy(X, _) :-
	X < 2,
	setNotInBattle(0),
	random(0, 2, Z),
	setEnemy(Z),
	!.
%% TIDAK MENEMUKAN ENEMY
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

%% PORTAL 1
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

%% PORTAL 2
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


% ENEMY STATS
%% ==============================================================
monsterEXPGOLD(XP, GOLD) :-
    XP is 500,
    GOLD is 900,
    enemy(wolf),!.

monsterEXPGOLD(XP, GOLD) :-
    XP is 300,
    GOLD is 500,
    enemy(goblin),!.

monsterEXPGOLD(XP, GOLD) :-
    XP is 200,
    GOLD is 300,
    enemy(slime),!.

monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(300*1.1**(LVL)),
    HP is round(400*1.1**(LVL)),
    enemy(goblin),!.
    
monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(200*1.1**(LVL)),
    HP is round(200*1.1**(LVL)),
    enemy(slime),!.

monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(400*1.1**(LVL)),
    HP is round(400*1.1**(LVL)),
    enemy(wolf),!.


% CHARACTER STAT
%% ==============================================================
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


% CHARACTER LEVEL AND STATUS
%% ==============================================================
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



wr(Line) :-
    write(Line),nl.

gambar(goblin) :-
    % goblin                                              
    wr('                    .....`      ``                '),
    wr('                `-+so++//-`  `//`                 '),
    wr('                `/+oyyoo+//:.:++.                 '),
    wr('             -:./ssoooshdy+o/+o+`           /os+/`'),
    wr('              -shhydh++ss+///+++           /dhssy:'),
    wr('               .ossyys+o+o/hh+/:            /mdds '),
    wr('                `yhhhhsoohy+oso/:.`     ``.+yhdh: '),
    wr(' `....`          -+hdyss+::+o++/::::-   .-//yoo:  '),
    wr(' .:yyo:/o:-  `.-+shddhyyo++oso+/:::/++.  .:oo+`   '),
    wr('  -ysyyydsy+oyyhyyyhysyhdhsso+::::/oyyh/ -o+/:    '),
    wr('   `-`+hyohydddddhys+:-/yssoo+////+omdddys+so:    '),
    wr('       -//:`.---..:hdo/+hyyso++++++ -shddmh+.     '),
    wr('                   sdhys+/:::--://+.   `oh:       '),
    wr('                   yddys:-:/:::///+/  :syo        '),
    wr('                   +dddhhysoo++++oo/   `.         '),
    wr('                   /dmmmmmmddhhhhhh:              '),
    wr('                  -syhdmmmmddddhhhd-              '),
    wr('                 `ssyhhdmmmmddddhhd+              '),
    wr('                 +hhyyhh/---/smmdhhs-             '),
    wr('                 .mmhyo.     `ommdy/`             '),
    wr('                 ohhs        :hmd/`               '),
    wr('               `sddhh        .mmddds:--:.         '),
    wr('               shhdy.         /ooosyyhyo          ').


gambar(slime) :-
    wr('                        ``              '),
    wr('             ````..---.`                '),
    wr('     `     ``.``...-:///:`              '),
    wr('    `.-.` `..--:--:::///++-   `.--      '),
    wr('    `..::`.-:s+s:::/so+/++s: `:///.     '),
    wr('     `.:/-.-os++///+++o+++os--+o+.      '),
    wr('      `-//--:/+o++/ysoso+soys+ss:       '),
    wr('      `.:/:-+sshys/ddddhsysysyo+`       '),
    wr('       `.---:+sys//+ossooosssssy.       '),
    wr('        `.-:/+++++oooooosssssyyyy+-`    '),
    wr('      `.-://+oooooooooooossssyyyyhh.    '),
    wr('  ```.-:://+++oossssssyyyyyhhhhhhhhd+.  '),
    wr('    ````..:::::///+/-:://oo+oyhyyyys/`  ').



gambar(naga):-                                                                                                    
    wr('                                      `...-/++:--.`                                                 '),
    wr('                             `-/osyhhddddddddddddddhyo/-.`                                          '),
    wr('                         -+sdmmmmmmddddddmmmmmmmmmmddmddhhyooo`                                     '),
    wr('                    `:ohmmmmmmmmmmmmmmmmmmmmmmmNNNNmmdddyshdddhs+/:/:                               '),
    wr('                 -ohNNNNNmmmmmmmmmmmmmmmmNNNNNNNNNNNmdhyhhhhhhyyyyhdy:                              '),
    wr('              .odNNNNNmmmdmmmmmmmmmmmmmmmmmmmmNNNMNhhddddddhhhhddhhdmy-                             '),
    wr('            :hNNNmmmmmmNNNNNNNNNNNNNNNNNNNNNNNNNMmdmmmmdddhhhhhhhhdddds.                            '),
    wr('         .smNmyo/--.-+mNNmmNNNNNNNNNNNNNNNNmmmmNdmNmNNmmNmdhddddhyyhhdh+/:-                         '),
    wr('       -ydo/.        `mNNmNNNNNNNNNNNNmmmmmNNNNhNNmmmmNNmmmmmmdhddhyyhdh+yhy/`                      '),
    wr('     -+/`           :mNNNNNmmmmNNNNNNNNNNNNNNmdmmmmmmmmNNNNmmmmmmhdyosyhs/ssyhs:                    '),
    wr('   `-.            .yNNNNmmmNNNNNNNNNNNNmmmmNNmmmmmddmmNNNmdmmmNmmmmyo++ss//+oosys/`                 '),
    wr('   `            `sNNmy+-```./mmmmmmmmmmmmNNNmmmddmmmmmdddmdhhyoo/oho- `:oo::`-/oooo:                '),
    wr('              `omho-        -mdmmdmmmmNNNmmdddmNNmmmmmmmmmy:mhsoyd/.`   `:o-`  `-+o++.              '),
    wr('            `+y/`          -dmdmdmmNNNNNmmmNNNNmmdddddmNNmdhmdyyyho       `-.`    `-://.            '),
    wr('           :+.            +mmmmmmNmNNmmmNNNNNmmmdhy++mMMNNmmmmdssh+         //       `.//.          '),
    wr('         ::             .yNmmmmNNNNmdmmNNNNmmddhs/..mNNNNmhmdddyyyo`        .+          `::`        '),
    wr('       .-              /mNNNNNNNNddmmdNNNmmddhs:   sNMNNNNhddhdyys/          :            `.:       '),
    wr('      .`             `yNNNNNNNmddNNmdmNNNmddhs`   :mNNNNNmNmdhys+/           .              `-      '),
    wr('     `              :mNNNNNNNddNMMNmddNNNmmdds-   oNNNmmNNmmdm+o/                             `     '),
    wr('                  -yNMNNNNmdddmmNNNNNmddmdddhyo+-.hNNmmdmdyhdh:+-                                   '),
    wr('                .yNNNmmmmmNNNmmmmmddddhhyysooo++++hmmmdhdhs/o/-`                                    '),
    wr('              `sNNNmhysohMNNNNNNNNNNmdddddddhhyyso/shhhhyds/::                                      '),
    wr('             +mNho:      +MNNNNNNNNNNNNNNmmmmmmmmdhhyoosyhs/++`                                     '),
    wr('           :dh+.         .MMNNmmNNNNmNmmNmmNNNNNmmmdhymdhhy+so-                                     '),
    wr('         -o+`            -NMNNdNNNmNNNNmmmmNmmNNmNmdyody++do/o:                                     '),
    wr('       `-.               /NNNmmNNNmmmmmmmmmmNNNNNmdy+yyo/.y/.o-                                     '),
    wr('       `               -hNNNNNNNNNmNmdhddhdmNNNNmmyoyds+/so/+:                                      '),
    wr('                     .yNMNNNNNmmmmmmNmdhyyhmNNNNmh+smmhyhsyo-                                       '),
    wr('                   `sNNNNNNNmdyohmNNNNNNmmNNNmNNmyodhs+yhho`                                        '),
    wr('                  /NMNNNNNmmdyosdmmdddmmmmmmmmNNmds+oymNms:                                         '),
    wr('                 sMMNmmmNNmdso:+oo+``/sddddddhydNNdsohNNd+-                                         '),
    wr('               `hMNNmhyNmds+/`        sNNNdyo/` -sNmhosdh+/-`                                       '),
    wr('               hMNNmyhmhyo/.         oMNdho:.``.--/hNdyoo/oyso/:`                                   '),
    wr('              +MNNmhoNdss.         .dNmhso++oo/:--...ddo//hmhso//:.                                 '),
    wr('              yMNNmssNhss`    ``.:/sdNdho/:-`       .Nms//-...``                                    '),
    wr('              oMMNmyhNhooo+::/+oyhyohmmys/`         `Nmyoo`                                         '),
    wr('               sMNNmmmmo+hsyysso/.`/s+mmsyoo+++:   /Ndho+hso/o/::/`                                 '),
    wr('                ./ommNNys+:.`      .` .dNdys+ooo   /mdysyhdhhy//oo+/`                               '),
    wr('                  :NNNNhsss-           `::-:+.      `:-`-:::/+: `:/.`                               '),
    wr('                  :so-Ny:.:`                                                                        '),
    wr('                      :/                                                                            ').
                                                                                                        

gambar(running) :-
    wr('                         :ymdy-         '),
    wr('                        oMMMMMN-        '),
    wr('                        mMMMMMMd        '),
    wr('                        -mMMMMMN.       '),
    wr('                         +MMMMMs        '),
    wr('                    `-/+hNMMMMNs` ` ..  '),
    wr('             ./oydmmmNNMMMMMMMMMh`o+NNh.'),
    wr('            +NNMMMMMNMMMMMMMMMMMMs.hMMd`'),
    wr('            oMMm----.NMMMMMMMMMMMMdoMN- '),
    wr('            /MMo     dMMMMMMMMMNmMMMM+  '),
    wr('            .NM+     sMMMMMMMMMh.hNN+   '),
    wr('             hMs     -MMMMMMMMMo `..    '),
    wr('             :MNo+-` `NMMMMMMMm`        '),
    wr('             `yNMN-` :MMMMMMMM/         '),
    wr('               //.  `dMMMMMMMd`         '),
    wr('                    sMMMMMMMMy:-`       '),
    wr('                    yMMMMMMMMMMNds:.    '),
    wr('                   `dMMMMMMMMMMMMMMmy:` '),
    wr('                  `hMMMMMMNmNMMMMMMMMMd-'),
    wr('                  sMMMMMMd:.-/+sydNMMMMd'),
    wr('              ``.+NMMMMNs`       -NMMMN:'),
    wr('           .+yhdmMMMMNy-         yMMMN: '),
    wr('   `   `.+yNMMMMNNNmh-           mMMMo  '),
    wr('  sdyoydNNNNmhs+:-.`            `NMMh   '),
    wr(' `NMMNNdo/-.                    :MMd`   '),
    wr('`hMNmo`                         hMM-    '),
    wr('yNy-                           oMMN`    '),
    wr('h+                            +MMMMh    '),
    wr('                              .odMMMy.  '),
    wr('                                 -shmmy ').


