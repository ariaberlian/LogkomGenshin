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

:- dynamic(equipedAcc/4).
:- dynamic(equipedWeap/4).
:- dynamic(equipedHead/4).
:- dynamic(equipedBoots/4).
:- dynamic(equipedArmor/4).
:- dynamic(equipedPOT/1).
:- dynamic(currentHP/1).

:- dynamic(skill/1).
:- dynamic(skillCDCounter/1).
:- dynamic(ultCDCounter/1).
:- dynamic(enemyCurrentHP/1).
:- dynamic(questFunc/3).
:- dynamic(questCounter/1).
:- dynamic(adaQuest/1).
:- dynamic(isDragonDead/1).




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


%% WOLF AREA
pos(wolfArea, 4, 16).

pos(obstacle, 3, 12).
pos(obstacle, 4, 12).
pos(obstacle, 5, 12).
pos(obstacle, 6, 12).
pos(obstacle, 7, 12).
pos(obstacle, 3, 13).
pos(obstacle, 7, 13).
pos(obstacle, 3, 14).
pos(obstacle, 7, 14).
pos(obstacle, 3, 15).
pos(obstacle, 7, 15).
pos(obstacle, 3, 16).
pos(obstacle, 5, 16).
pos(obstacle, 6, 16).
pos(obstacle, 7, 16).


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
weapon('Baseball Bat', 'None', 'atk', '+', 40).

% armor(name, stat, statType, statNum)
armor('Wooden Armor', 'hp', '+', 400).
armor('Silver Armor', 'hp', '%', 10).
armor('Golden Armor', 'hp', '%', 30).
armor('Divine Protection', 'hp', '%', 50).
armor('Uniqlo Tshirt', 'hp', '+', 0).

% head(name, stat, statType, statNum)
head('Kreuzeck Bag Helmet', 'def', '+', 30).
head('Cursed Helmet', 'def', '%', 20).
head('Sky Guardian Helmet', 'def', '+', 8000).
head('Honda SNI Helmet', 'def', '%', 50).
head('Baseball Helmet', 'def', '+', 0).

% boots(name, stat, statType, statNum).
boots('Wooden Bakiak', 'hp', '+', 150).
boots('Lightning Vans', 'hp', '%', 5).
boots('Adidas x Nike', 'hp', '+', 4000).
boots('Holly Swallow', 'hp', '%', 20).
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
grade('SS', 'Reforged Sandalwood Warden Staff', 6).
grade('SS', 'Divine Protection', 7).
grade('SS', 'Honda SNI Helmet', 8).
grade('SS', 'Holly Swallow', 9).
grade('SS', 'Fiesta SpicyChikenWings', 10).



%% MAIN MENU
%% ==============================================================

%% Setelah dicompile, maka user harus melakukan "start." terlebih dahulu
start :-
    \+ started(true),
	retract(pos(player, _, _)),
	retract(enemy(_)),
	retract(notInBattle(_)),
	retract(started(_)),
    retractall(jobFound(_)),
    retractall(questFunc(_,_,_)),
    asserta(questFunc(0,0,0)),
    retractall(currentHP(_)),
    retractall(ekspi(_)),
    retractall(gold(_)),
    retractall(job(_)),
    retractall(adaQuest(_)),

    asserta(job(programmer)),
    asserta(ekspi(0)),
    asserta(gold(1000)),
    finalSTATS(_,_,_,HP),
    asserta(currentHP(HP)),
	asserta(pos(player, 1, 1)),
	asserta(enemy(null)),
	asserta(notInBattle(true)),
	asserta(started(true)),
    asserta(questCounter(1)),
    asserta(adaQuest(false)),
    asserta(jobFound(false)),
    % EQUIPMENT & BAG
    asserta(bag(['Baseball Bat', 'Uniqlo Tshirt', 'Baseball Helmet', 'Sendal Swallow', 'Ali Ring','Health Potion Small','Health Potion Small','Health Potion Small','Health Potion Small','Health Potion Small'])),
    asserta(equipedWeap('Baseball Bat', 'atk', '+', 1)),
    asserta(equipedArmor('Uniqlo Tshirt', 'hp', '+', 0)),
    asserta(equipedHead('Baseball Helmet', 'def', '+', 0)),
    asserta(equipedBoots('Sendal Swallow', 'hp', '+', 1)),
    asserta(equipedAcc('Ali Ring', 'atk', '+', 1)),


	write('Game is Starting...'), nl,
    write('Youkouso'), nl,
    write('  sSSSs   d sss   d s  b   sss. d    d d d s  b        sss. d sss   d     S d s.   d'), nl,
    write(' S     S  S       S  S S d      S    S S S  S S      d      S       S    P  S  ~O  S'), nl,
    write('S         S       S   SS Y      S    S S S   SS      Y      S       Ssss\'   S   `b S'), nl,
    write('S         S sSSs  S    S   ss.  S sSSS S S    S        ss.  S sSSs  S   s   S sSSO S'), nl,
    write('S    ssSb S       S    S      b S    S S S    S           b S       S    b  S    O S'), nl,
    write(' S     S  S       S    S      P S    S S S    S           P S       S     b S    O S'), nl,
    write('  "sss"   P sSSss P    P ` ss\'  P    P P P    P      ` ss\'  P sSSss P     P P    P P'), nl, nl,

    wr('Pada suatu hari yang cerah di negara Belzard, hiduplah seorang pemuda biasa yang sedang asik bermain baseball'),
    wr('bersama temannya. *Ctankk* "HOMERUNNNN!!!". Seorang anak teriak. Lalu pemuda bernama Issei berlari mengejar'),
    wr('bola itu meskipun sudah keluar dari arena bermain. Dia terus berlari hingga berpapasan dengan truck-kun.'),
    wr('Issei membuka mata. Dia melihat sesosok wanita cantik di depannya.Wanita itu berkata, [Ara~ara aramaa~,'),
    wr('Sekarang kau punya dua pilihan, yaitu untuk pergi ke surga atau menjalani hidup baru di dunia yang baru.]'),
    wr('Tanpa pikir panjang, Issei menjawab, "Aku akan menjadi raja harem di dunia yang baru itu!".'), nl,
    wr('[Jelajahilah dunia ini....'),
    wr('temuilah Aku jika kamu sudah siap]'),
    wr('...'),
    write('>> '),
    read(_),
    wr('...Dewi itu menghilang seketika'),
    wr('"tunggu!!! dimana aku harus mencarimu?"'),
    wr('...'),
    
    nl,

    help,nl,

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
	pos(wolfArea, X, Y),
	write('W'),
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
concat([],L2,L2).
concat([H|T], L2, [H|L3]):-concat(T,L2,L3).

push(Element,Queue,Result):- concat(Queue,[Element],Result).

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
    write('Apa nama item yang ingin anda buang? (ketik kembali untuk membatalkan)'),nl,
    bag(B),
    write('>> '),
    read(Input),
    (
        Input = kembali ->!,nl, inventory;
        \+ member(Input, B)-> !,write('Item yang anda masukan tidak ada dalam inventory.'),nl,nl, inventOption(1); 
        write('Anda membuang '), write(Input),nl,throw(Input, B, Res),retract(bag(_)),asserta(bag(Res))
    ).

inventOption(2) :-
    write('Apa nama item yang ingin anda gunakan? (ketik kembali untuk membatalkan)' ),nl,
    bag(B),
    write('>> '),
    read(Input),
    ( 
        Input = kembali ->!,nl, inventory;
        \+ member(Input, B) ->!, write('Item yang anda masukan tidak ada dalam inventory.'),nl,nl,inventOption(2);
         detail(Input, Jenis, Stat, StatType, StatNum),job(Job),
        (Jenis = potion -> usePot(Input, Stat, StatNum) ; useEquip(Input, Jenis, Stat, StatType, StatNum, Job))
    ),
    write('Item '), write(Input), write(' telah digunakan!'),nl.

inventOption(X):- 
    X > 2,
    !,fail.

useEquip(Name, Jenis, Stat, StatType, StatNum, Job) :- 
    (
        (Jenis = 'Single-Handed Sword'; Jenis = 'Greatsword'), job(Job), Job = swordsman -> retractall(equipedWeap(_,_,_,_)),asserta(equipedWeap(Name, Stat, StatType, StatNum));
        (Jenis = 'Bow' ; Jenis = 'Crossbow'), job(Job), Job = archer -> retractall(equipedWeap(_,_,_,_)),asserta(equipedWeap(Name, Stat, StatType, StatNum));
        (Jenis = 'Book'; Jenis = 'Staff'), job(Job), (Job =  wizard ; Job = priest) -> retractall(equipedWeap(_,_,_,_)), asserta(equipedWeap(Name, Stat, StatType, StatNum));
        Jenis = 'armor' -> retractall(equipedArmor(_,_,_,_)), asserta(equipedArmor(Name, Stat, StatType, StatNum));
        Jenis = 'head' -> retractall(equipedHead(_,_,_,_)), asserta(equipedHead(Name, Stat, StatType, StatNum));
        Jenis = 'boots' -> retractall(equipedBoots(_,_,_,_)), asserta(equipedBoots(Name, Stat, StatType, StatNum));
        Jenis = 'accessory' -> retractall(equipedAcc(_,_,_,_)), asserta(equipedAcc(Name, Stat, StatType, StatNum))
    ),!.

usepotion :-
    wr('Potion nomor berapa yang akan anda gunakan?'),
    wr('1. Health Potion Small, Efek: Recover 15% Hp'),
    wr('2. Health Potion Medium, Efek: Recover 25% Hp'),
    wr('3. Health Potion Large, Efek: Recover 50% Hp'),
    wr('4. Rage Potion, Efek: atk +10% selama 3 turn'),
    wr('5. Smart Potion, Efek: int +10% selama 3 turn'),
    wr('6. Rock Potion, Efek: def +10% selama 3 turn'),
    write('>> '),
    read(Input),
    (
    Input = 1 -> usePot('Health Potion Small', 'recover Hp', 15);
    Input = 2 -> usePot('Health Potion Medium', 'recover Hp', 25);
    Input = 3 -> usePot('Health Potion Large', 'recover Hp', 50);
    Input = 4 -> usePot('Rage Potion', 'atk', 10);
    Input = 5 -> usePot('Smart Potion', 'int', 10);
    Input = 6 -> usePot('Rock Potion', 'def', 10)
    ),!.

usePot(Name, Stat, StatNum) :-
    bag(Bag),
    count(Name, Bag, Num),
    finalSTATS(_,_,_, MAXHP),
    (
        Num = 0 -> write('Potion tidak ada dalam inventory! Haha panik panik dia panik');
        throw(Name, Bag, Res),retract(bag(_)),asserta(bag(Res)),
        (Stat = 'recover Hp' -> HPRegenerated is round(MAXHP*(StatNum/100)+ 0.01), fungsiHeal(HPRegenerated, NewHP), write('HP Anda terpulihkan sebanyak: '),wr(NewHP);true),
        (Stat = 'atk' -> retract(equipedPOT(_)), asserta(equipedPOT(atk)) ; true),
        (Stat = 'def' -> retract(equipedPOT(_)), asserta(equipedPOT(def)) ; true),
        (Stat = 'int' -> retract(equipedPOT(_)), asserta(equipedPOT(int)) ; true)
    ),!.




%% STORE
%% ==============================================================
jajan(M, P, Result) :-
    M >= P, Result is M-P,!.
jajan(M, P, _Result) :-
    M < P, !,write('Gold anda tidak mencukupi!'),nl,fail.

store :-
    nl,
    pos(player, 7, 2),
    write('Selamat datang di store. Ingin belanja apa?'),nl,
    gold(Gold),
    write('Gold anda saat ini: '), write(Gold),nl,
    write('1. Pandora Box 300G'),nl,
    write('2. Potion'),nl,
    write('3. Keluar'),nl,
    write('Masukkan nomor yang dipilih'),nl,
    write('>> '),
    read(Input),
    storeOpt(Input),!.

store2 :-
    write('Terimakasih telah membeli! Ingin membeli barang lagi?'),nl,
    gold(Gold),
    write('Gold anda saat ini: '), write(Gold),nl,
    write('1. Pandora Box 300G'),nl,
    write('2. Potion'),nl,
    write('3. Keluar'),nl,
    write('Masukkan nomor yang dipilih'),nl,
    write('>> '),
    read(Input),
    storeOpt(Input),!.

storeOpt(1) :-
    gold(M),
    jajan(M, 300, SisaUang),
    random(1, 101, N),
    random(1, 11, N1),
    gacha(Eq, Grade, N, N1),
    retractall(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Pandora Box.'),nl,
    write('Membuka Pandora Box....'),nl,
    write('Anda mendapatkan: '),write(Eq),write(' dengan tingkat kelangkaan '),write(Grade),
    bag(Y),
    push(Eq, Y, B),
    retract(bag(_)),
    asserta(bag(B)),nl,!,nl,
    store2.

storeOpt(2) :-
    write('1. Health Potion Small  50G'),nl,
    write('2. Health Potion Medium 100G'),nl,
    write('3. Health Potion Large  200G'),nl,
    write('4. Rage Potion 200G'),nl,
    write('5. Smart Potion 200G'),nl,
    write('6. Rock Potion 200G'),nl,
    write('7. kembali'),nl,
    write('>> '),
    read(Input),
    buyPot(Input),!,nl,
    store2.

storeOpt(X) :-
    X > 2,
    nl,
    wr('Anda keluar dari toko'),!,true.


gacha(Eq,'B', N, N1) :-
    N >= 1, N < 66 -> grade('B', Eq, N1),!.

gacha(Eq,'A', N, N1) :-
    N >= 66, N < 87 -> grade('A', Eq, N1),!.

gacha(Eq,'S', N, N1) :-
    N >= 87, N < 96 -> grade('S', Eq, N1),!.

gacha(Eq,'SS', N, N1) :-
    N >= 96, N =< 100 -> grade('SS', Eq, N1),!.

buyPot(1) :- 
    gold(M),
    jajan(M, 50, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Health Potion Small.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Small', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(2) :- 
    gold(M),
    jajan(M, 100, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Health Potion Medium.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Medium', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(3) :- 
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Health Potion Large.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Health Potion Large', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(4) :- 
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Rage Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Rage Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(5) :- 
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Smart Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Smart Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(6) :- 
    gold(M),
    jajan(M, 200, SisaUang),
    retract(gold(_)),
    asserta(gold(SisaUang)),
    write('Anda telah membeli Rock Potion.'),nl,
    write('Potion dimasukkan ke dalam inventory'),nl,
    bag(Y),
    push('Rock Potion', Y, B),
    retract(bag(_)),
    asserta(bag(B)),
    !.

buyPot(X) :-
    X > 6, nl,
    store,!.

absolute(X,Y):-
    Y is round(sqrt(X**2)).

randomEnemyLevel(MyLevel) :-
    Low is MyLevel - 3,
    High is MyLevel + 3,
    random(Low,High,Lev), % RANDOMIZE LEVEL MONSTER +- 3 dari level kita
    absolute(Lev, EnemyLevel),
    retractall(enemyLevel(_)),
    asserta(enemyLevel(EnemyLevel)).


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
	write('Issei begerak ke utara'),
	!.
writeArah(M) :-
	M = 1,
	write('Issei bergerak ke selatan'),
	!.
writeArah(M) :-
	M = 2,
	write('Issei bergerak ke barat'),
	!.
writeArah(M) :-
	M = 3,
	write('Issei bergerak ke timur'),
	!.

%% WOLF
setEnemy(X) :-
	X = 0,
	level(CurrentLVL),
    randomEnemyLevel(CurrentLVL),
    enemyLevel(EnemyLevel),
	write('You found a level '), write(EnemyLevel), write(' Wolf!!!!'), nl,
    gambar(wolf), nl,
	retract(enemy(_)),
	asserta(enemy(wolf)),
	monsterSTAT(_,HP),
    retractall(enemyCurrentHP(_)),
    asserta(enemyCurrentHP(HP)),
    retractall(skill(_)),
    retractall(skillCDCounter(_)),
    retractall(ultCDCounter(_)),
    asserta(skillCDCounter(2)),
    asserta(ultCDCounter(3)), nl, nl,
    attack,
	!.
%% SLIME
setEnemy(X) :-
	X = 1,
    level(CurrentLVL),
    randomEnemyLevel(CurrentLVL),
    enemyLevel(EnemyLevel),
	write('You found a level '), write(EnemyLevel), write(' Slime!!!!'), nl,
    gambar(slime),
	retract(enemy(_)),
	asserta(enemy(slime)),
    monsterSTAT(_,HP),
    retractall(enemyCurrentHP(_)),
    asserta(enemyCurrentHP(HP)),
    retractall(skill(_)),
    retractall(skillCDCounter(_)),
    retractall(ultCDCounter(_)),
    asserta(skillCDCounter(2)),
    asserta(ultCDCounter(3)),nl, nl,
    attack,
	!.
%% GOBLIN
setEnemy(X) :-
	X = 2,
	level(CurrentLVL),
    randomEnemyLevel(CurrentLVL),
    enemyLevel(EnemyLevel),
	write('You found a level '), write(EnemyLevel), write(' Goblin!!!!'), nl,
    gambar(goblin), nl,
	retract(enemy(_)),
	asserta(enemy(goblin)),
    monsterSTAT(_,HP),
    retractall(enemyCurrentHP(_)),
    asserta(enemyCurrentHP(HP)),
    retractall(skill(_)),
    retractall(skillCDCounter(_)),
    retractall(ultCDCounter(_)),
    asserta(skillCDCounter(2)),
    asserta(ultCDCounter(3)), nl,nl,
    attack,
	!.
%% DRAGON
setEnemy(X) :-
	X = 3,
	write('Is it the final battle??!!!'), nl,
    setNotInBattle(0),
    gambar(naga), nl,
    retractall(enemyLevel(_)),
    asserta(enemyLevel(50)),
	retract(enemy(_)),
	asserta(enemy(dragon)),
    monsterSTAT(_,HP),
    retractall(skill(_)),
    retractall(enemyCurrentHP(_)),
    asserta(enemyCurrentHP(HP)),
    retractall(skillCDCounter(_)),
    retractall(ultCDCounter(_)),
    asserta(skillCDCounter(2)),
    asserta(ultCDCounter(3)), nl, nl,
    attack,
	!.

%% MENEMUKAN ENEMY di WOLF AREA
foundEnemy(X, _) :-
	X < 2,
	inWolfArea,
	setNotInBattle(0),
	setEnemy(0),
	!.
	
%% MENEMUKAN ENEMY
foundEnemy(X, _) :-
	X < 2,
	random(0, 3, Z),
	setNotInBattle(0),
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

inWolfArea :-
	pos(player, X, Y),
	Y > 12,
	Y < 16,
	X > 3,
	X < 7.


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
	write('Are you afraid ?!!'), nl,
    gambar(running),
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

bukaStore(X,Y) :-
    pos(store, X, Y),
    store,!.

bukaQuest(X,Y) :-
    pos(quest,X,Y),
    questto,!.


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

    \+bukaStore(X1,Y1),
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
    \+bukaQuest(X1,Y1),
	randomEnemy(0), 
    !.

s :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X + 0,
	Y1 is Y + 1,
	notWall(X1, Y1),
	move(X1, Y1),

    \+bukaStore(X1,Y1),
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
    \+bukaQuest(X1,Y1),
	randomEnemy(1), 
    !.

a :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X - 1,
	Y1 is Y + 0,
	notWall(X1, Y1),
	move(X1, Y1),

    
    \+bukaStore(X1,Y1),
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
    \+bukaQuest(X1,Y1),
	randomEnemy(2),!. 

d :- 
	started(true),
	pos(player, X, Y),
	notInBattle(true),
	X1 is X + 1,
	Y1 is Y + 0,
	notWall(X1, Y1),
	move(X1, Y1),


    \+bukaStore(X1,Y1), 
	\+teleport(X1, Y1),
	\+boosPos(X1, Y1),
    \+bukaQuest(X1,Y1),
	randomEnemy(3),
    !.

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

monsterEXPGOLD(30000, 1500000) :-
    enemy(dragon),!.

monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(250*1.1**(LVL) + 0.00001),
    HP is round(250*1.1**(LVL) + 0.00001),
    enemy(goblin),!.
    
monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(200*1.1**(LVL) + 0.00001),
    HP is round(200*1.1**(LVL) + 0.00001),
    enemy(slime),!.

monsterSTAT(ATK, HP) :-
    enemyLevel(LVL),
    ATK is round(300*1.1**(LVL) + 0.00001),
    HP is round(300*1.1**(LVL) + 0.00001),
    enemy(wolf),!.

monsterSTAT(40000, 700000) :-
    enemy(dragon),!.


% CHARACTER STAT
%% ==============================================================
baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(swordsman),
    ATK is 200*(1.1**(LVL-1)),
    DEF is 300*(1.1**(LVL-1)),
    INT is 200*(1.1**(LVL-1)),
    HP is 2000*(1.1**(LVL-1)),!.
    

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(archer),
    ATK is 300*(1.1**(LVL-1)),
    DEF is 100*(1.1**(LVL-1)),
    INT is 200*(1.1**(LVL-1)),
    HP is 1500*(1.1**(LVL-1)),!.

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(wizard),
    ATK is 100*(1.1**(LVL-1)),
    DEF is 120*(1.1**(LVL-1)),
    INT is 300*(1.1**(LVL-1)),
    HP is 1700*(1.1**(LVL-1)),!.

baseSTAT(ATK, DEF, INT, HP) :-
    level(LVL),
    job(priest),
    ATK is 160*(1.1**(LVL-1)),
    DEF is 150*(1.1**(LVL-1)),
    INT is 300*(1.1**(LVL-1)),
    HP is 2200*(1.1**(LVL-1)),!.

baseSTAT(250, 100, 340, 2000) :-
    job(programmer),!.

baseSTAT(9999, 9999, 9999, 999999) :-
    job(god).


%% FINAL STATS CACLCULATION
%% =====================================================================================
finalSTATS(ATK, DEF, INT, MAXHP) :-
    finalATK(ATK1), finalDEF(DEF1), finalINT(INT1), finalHP(HP1),
    ATK is ATK1, DEF is DEF1, INT is INT1, MAXHP is HP1.

% FINAL ATK CALCULATION
finalATK(ATK) :-
    job(archer),
    skill(on),
    finalATK1(ATK1),
    ATK is ATK1*(1.5),!.
finalATK(ATK) :-
    finalATK1(ATK), !.

finalATK1(ATK) :-
    finalATK2(ATK1),
    equipedPOT(atk),
    ATK is ATK1*(1.1),!.
finalATK1(ATK) :-
    finalATK2(ATK),!.

finalATK2(ATK) :-
    finalATK3(ATK1),
    equipedAcc(_,'atk', '+', ATK2),
    ATK is ATK1 + ATK2,!.
finalATK2(ATK) :-
    finalATK3(ATK1),
    equipedAcc(_,'atk', '%', ATKPercent),
    baseSTAT(BASEATK,_,_,_),
    ATK2 is BASEATK*(ATKPercent/100),
    ATK is ATK1 + ATK2,!.
finalATK2(ATK) :-
    finalATK3(ATK2),
    ATK is ATK2,!.

finalATK3(ATK):-
    baseSTAT(ATK1,_,_,_),
    equipedWeap(_, 'atk', '+', StatNum),
    ATK is ATK1 + StatNum,!.
finalATK3(ATK):-
    baseSTAT(ATK1,_,_,_),
    equipedWeap(_, 'atk', '%', StatNum),
    ATK is ATK1*(1+(StatNum/100)),!.
finalATK3(ATK) :-
    baseSTAT(ATK,_,_,_),!.


% FINAL DEF CALCULATION
finalDEF(DEF) :-
    job(swordsman),
    skill(on),
    finalDEF1(DEF1),
    DEF is DEF1*2,!.
finalDEF(DEF) :-
    finalDEF1(DEF),!.

finalDEF1(DEF) :-
    finalDEF2(DEF1),
    equipedPOT(def),
    DEF is DEF1*(1.1),!.
finalDEF1(DEF) :-
    finalDEF2(DEF).

finalDEF2(DEF) :-
    finalDEF3(DEF1),
    equipedAcc(_, 'def','+', StatNum),
    DEF is DEF1 + StatNum,!.
finalDEF2(DEF):-
    finalDEF3(DEF1),
    baseSTAT(_,BASEDEF,_,_),
    equipedAcc(_, 'def', '%', StatNum),
    DEF2 is BASEDEF*(StatNum/100),
    DEF is DEF1 + DEF2,!.
finalDEF2(DEF) :-
    finalDEF3(DEF),!.

finalDEF3(DEF):-
    baseSTAT(_,DEF1,_,_),
    equipedHead(_, _,'+', StatNum),
    DEF is DEF1 + StatNum,!.
finalDEF3(DEF):-
    baseSTAT(_,DEF1,_,_),
    equipedHead(_, _, '%', StatNum),
    DEF is DEF1*(1+(StatNum/100)),!.
finalDEF3(DEF) :-
    baseSTAT(_,DEF,_,_),!.


% FINAL INT CALCULATION
finalINT(INT) :-
    finalINT2(INT1),
    equipedPOT(int),
    INT is INT1*(1.1),!.
finalINT(INT) :-
    finalINT2(INT),!.

finalINT2(INT) :-
    finalINT3(INT1),
    equipedAcc(_,'int', '+', INT2),
    INT is INT1 + INT2,!.
finalINT2(INT) :-
    finalINT3(INT1),
    equipedAcc(_,'int', '%', INTPercent),
    baseSTAT(_,_,BaseINT,_),
    INT2 is BaseINT*(INTPercent/100),
    INT is INT1 + INT2,!.
finalINT2(INT) :-
    finalINT3(INT2),
    INT is INT2,!.

finalINT3(INT):-
    baseSTAT(_,_,INT1,_),
    equipedWeap(_, 'int', '+', StatNum),
    INT is INT1 + StatNum,!.
finalINT3(INT):-
    baseSTAT(_,_,INT1,_),
    equipedWeap(_, 'int', '%', StatNum),
    INT is INT1*(1+(StatNum/100)),!.
finalINT3(INT) :-
    baseSTAT(_,_,INT,_),!.
    
% FINAL HP CALCULATION
finalHP(HP) :-
    finalHP2(HP1),
    equipedBoots(_,'hp', '+', HP2),
    HP is HP1 + HP2,!.
finalHP(HP) :-
    finalHP2(HP1),
    equipedBoots(_,'hp', '%', HPPercent),
    baseSTAT(_,_,_,BASEHP),
    HP2 is BASEHP*(HPPercent/100),
    HP is HP1 + HP2,!.
finalHP(HP) :-
    finalHP2(HP2),
    HP is HP2.

finalHP2(HP):-
    baseSTAT(_,_,_,HP1),
    equipedArmor(_, 'hp', '+', StatNum),
    HP is HP1 + StatNum,!.
finalHP2(HP):-
    baseSTAT(_,_,_,HP1),
    equipedArmor(_, 'hp', '%', StatNum),
    HP is HP1*(1+(StatNum/100)),!.
finalHP2(HP) :-
    baseSTAT(_,_,_,HP),!.


%% Battle Mode
%% =======================================================

attack :-
    notInBattle(false),
    enemyCurrentHP(EnemyHP),
    enemy(Monster),
    enemyLevel(LVL),
    skillCDCounter(SkillCD),
    ultCDCounter(ULTCD),
    monsterSTAT(_, MAXHP),
    write(Monster), write(' '), write(' evel: '), write(LVL), nl,
    write('Darah Monster: '), write(EnemyHP), write('/'), write(MAXHP), nl,
    wr('1. Normal attack'),
    wr('2. Use Skill'),
    wr('3. Burst'),
    wr('4. UsePotion'),
    wr('5. status'),
    wr('6. run'),
    write('>> '),
    read(Input),
    attackOption(Input, DMGDEALT, SkillCD, ULTCD),
    fungsiCritikal(DMGDEALT, FINALDAMAGE),
    ZDAMAGE is round(FINALDAMAGE + 0.0001),
    write('Anda Memberikan '), write(ZDAMAGE), write(' damage!'), nl, nl,
    
    pengecekanEnemyHP(ZDAMAGE),
    % NewSkillCooldown is SkillCD - 1,
    % NewULTCooldown is ULTCD -1,
    % retractall(skillCDCounter(_)),
    % retractall(ultCDCounter(_)),
    % asserta(New)
    !.

pengecekanCD:-
    skillCDCounter(CurrentSCD),
    ultCDCounter(CurrentULTCD),
    NewSCD is CurrentSCD - 1,
    NewUCD is CurrentULTCD -1,
    pengecekanApakahSCD0(NewSCD),
    pengecekanApakahUCD0(NewUCD),!.

pengecekanApakahSCD0(SkillCD) :-
    SkillCD = 0,
    retractall(skill(_)),
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(SkillCD)),!.
pengecekanApakahSCD0(SkillCD) :-
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(SkillCD)),!.

pengecekanApakahUCD0(SkillCD) :-
    retractall(ultCDCounter(_)),
    asserta(ultCDCounter(SkillCD)),!.


attackOption(1, DMGDEALT, _, _) :-
    job(swordsman),
    finalSTATS(ATK,_,_,_),
    DMGDEALT is ATK*(0.5),!.
attackOption(1, DMGDEALT, _, _) :-
    job(archer),
    finalSTATS(ATK,_,_,_),
    DMGDEALT is ATK*(0.5),!.
attackOption(1, DMGDEALT, _, _) :-
    job(wizard),
    finalSTATS(_,_,INT,_),
    DMGDEALT is INT*(0.5),!.
attackOption(1, DMGDEALT, _, _) :-
    job(priest),
    finalSTATS(_,_,INT,_),
    DMGDEALT is INT*(0.15),!.
attackOption(1, DMGDEALT, _, _) :-
    finalSTATS(ATK,_,_,_),
    DMGDEALT is ATK*(0.6),!.


attackOption(2, DMGDEALT, SkillCD, _) :-
    job(swordsman),
    SkillCD =< 0,
    retractall(skill(_)),
    asserta(skill(on)),
    DMGDEALT is 0,
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(2)),
    !.
attackOption(2, DMGDEALT, SkillCD, _) :-
    job(archer),
    SkillCD =< 0,
    retractall(skill(_)),
    asserta(skill(on)),
    DMGDEALT is 0,    
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(2)),
    !.
attackOption(2, DMGDEALT, SkillCD, _) :-
    job(wizard),
    SkillCD =< 0,
    finalSTATS(_,_,INT,_),
    DMGDEALT is INT,
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(2)),
    !.
attackOption(2, DMGDEALT, SkillCD, _) :-
    job(priest),
    SkillCD =< 0,
    finalSTATS(_,_,INT,_),
    Heal is INT*(3.5),
    fungsiHeal(Heal, Regen),
    write('Meregenerasi '), write(Regen), wr(' HP'),
    DMGDEALT is 0,
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(2)),
    !.
attackOption(2, DMGDEALT, SkillCD, _) :-
    SkillCD =< 0,
    finalSTATS(ATK,_,_,_),
    DMGDEALT is ATK,
    retractall(skillCDCounter(_)),
    asserta(skillCDCounter(2)),
    !.
attackOption(2, _, SkillCD, _) :-
    SkillCD > 0,
    wr('Skill dalam Cooldown'), nl, !, attack.

attackOption(3,DMGDEALT,_,ULTCD) :-
    job(wizard),
    ULTCD =< 0,
    finalSTATS(ATK,_,INT,_),
    DMGDEALT is 4*sqrt(ATK*INT),
    retractall(ultCDCounter(_)),
    asserta(ultCDCounter(3)),
    !.
attackOption(3,DMGDEALT,_,ULTCD) :-
    job(priest),
    ULTCD =< 0,
    finalSTATS(_,_,INT,_),
    DMGDEALT is INT*2,
    Heal is INT*(0.5),
    fungsiHeal(Heal, Regen),
    write('Meregenerasi '), write(Regen), wr(' HP'),
    retractall(ultCDCounter(_)),
    asserta(ultCDCounter(3)),
    !.
attackOption(3,DMGDEALT,_,ULTCD) :-
    ULTCD =< 0,
    finalSTATS(ATK,_,_,_),
    DMGDEALT is 2*ATK,
    retractall(ultCDCounter(_)),
    asserta(ultCDCounter(3)),
    !.
attackOption(3,_,_,ULTCD) :-
    ULTCD > 0,
    wr('Burst dalam Cooldown'), !, attack, fail.

attackOption(4,_,_,_) :-
    usepotion,nl, nl, attack,!.

attackOption(5,_,_,_) :-
    status, nl, nl, attack,!.

attackOption(6,0,_,_) :-
    random(1,5,N),
    (
    N = 1 -> !,nl, wr('Gagal Melarikan Diri!!'),nl;
    N > 1 -> !, wr('Anda Telah Melarikan Diri'), !, run, fail
    ),!.

attackOption(_,_,_,_) :-
    wr('Input salah'), nl, attack,!.


fungsiHeal(Gain, Regen) :-
    finalSTATS(_,_,_,MAXHP),
    currentHP(CurrentHP),
    NewHP is CurrentHP + Gain,
    MAXHP > NewHP,
    Regen is Gain,
    retractall(currentHP(CurrentHP)),
    asserta(currentHP(NewHP)),!.
fungsiHeal(Gain, Regen) :-
    finalSTATS(_,_,_,MAXHP),
    currentHP(CurrentHP),
    NewHP is CurrentHP + Gain,
    MAXHP =< NewHP,
    Regen is (MAXHP - CurrentHP),
    retractall(currentHP(CurrentHP)),
    asserta(currentHP(MAXHP)),!.

fungsiCritikal(DMGDEALT,FINALDAMAGE) :-
    random(1, 5, N),
    (
        N = 1 -> !, FINALDAMAGE is DMGDEALT*2, wr('KRITIKAL!!!');
        N > 1 -> !, FINALDAMAGE is DMGDEALT
    ),!.

pengecekanEnemyHP(DMGDEALT) :-
    enemyCurrentHP(EnemyHP),
    NewEnemyHP is (EnemyHP - DMGDEALT),
    monsterEXPGOLD(XP1, GOLD1),
    ekspi(XP),
    gold(GOLD),
    level(MYLEVEL),
    OldLevel is MYLEVEL,
    enemyLevel(LVL),
    monsterSTAT(ATK, _),
    Low is ATK - (10*(1.05)**LVL),
    High is ATK + (10*(1.05)**LVL),
    random(Low, High, EnemyDMG),
    enemy(MONSTER),
    questFunc(X,Y,Z),
    (
        NewEnemyHP =<0, enemy(dragon) -> !, asserta(isDragonDead(true)), wr('Anda telah mengalahkan NAGA!'),wr('.'),wr('.'),wr('.'),wr('Ketik apa saja untuk melanjutkan...'), write('>> '),read(_),wr('Anda mendekati naga itu...'),write('>> '),
        read(_),wr('"Kemarilah bocah...." sang naga berbicara'),write('>> '),read(_),wr('Kalahkan dewi itu, atau...'), write('>> '), read(_), wr('Uhhuk-uhhuk-uhhuk...'), write('>> '),read(_),wr('atau kau akan ................'),nl,nl,nl,wr('COMING "VERY" SOON, GENSHIN SEKAI II Forgotten God'),wr('~Fin'), asserta(isDragonDead(true)),fail;
        NewEnemyHP =< 0 -> !, NewGold is GOLD + GOLD1, NewXP is XP + XP1, retractall(ekspi(_)), retractall(gold(_)), asserta(gold(NewGold)), asserta(ekspi(NewXP)), level(NewLevel), fungsiRefreshDarah(OldLevel, NewLevel), retractall(enemyCurrentHP(_)), wr('Musuh telah mati,'), write('Anda Mendapatkan '), write(GOLD1), write(' Gold & '), write(XP1), wr('EXP!'),setNotInBattle(1),
        (
            MONSTER = goblin -> !, X1 is X-1, retractall(questFunc(_,_,_)), asserta(questFunc(X1,Y,Z));
            MONSTER = slime -> !, Y1 is Y-1, retractall(questFunc(_,_,_)), asserta(questFunc(X,Y1,Z));
            MONSTER = wolf -> !, Z1 is Z-1, retractall(questFunc(_,_,_)), asserta(questFunc(X,Y,Z1))
        ), fail;
        NewEnemyHP > 0 -> !, retractall(enemyCurrentHP(_)), asserta(enemyCurrentHP(NewEnemyHP)),kenaSerangBro(EnemyDMG)
    ),!.


kenaSerangBro(DMGDEALT) :-
    currentHP(HP),
    % PENGURANGAN DMGDEALT DENGAN DEF
    finalSTATS(_,DEF,_,_),
    DMGTAKEN is DMGDEALT - (DEF/4),
    NewHP is HP - DMGTAKEN,
    enemy(ENEMY),
    DMGTAKEN2 is round(DMGTAKEN + 0.0001),
    (
        NewHP =< 0 -> !, retractall(currentHP(_)), asserta(currentHP(0)), retractall(gold(_)), asserta(gold(0)), wr('Anda Telah Mati'), wr('"Kemampuanmu hanya seginikah, aku menyesal membawamu kemari."'), wr('"Bocah lemah sepertinya tidak cocok untuk menggapai harapanku."'), retractall(notInBattle(_)), setNotInBattle(1), retractall(started(_)), asserta(started(false)),fail;
        NewHP > 0 -> !, retractall(currentHP(_)), asserta(currentHP(NewHP)), write(ENEMY), wr(' menyerang!!!'), write('Anda terkena '), write(DMGTAKEN2),
        wr(' damage!'), nl,pengecekanCD,attack
    ),!.

fungsiRefreshDarah(OldLevel, NewLevel) :-
    NewLevel > OldLevel,nl,
    wr('LEVEL UP!!!!'),
    currentHP(CurrentHP),
    finalSTATS(_,_,_,MAXHP),
    retractall(currentHP(CurrentHP)),
    asserta(currentHP(MAXHP)), nl,!.
    
fungsiRefreshDarah(_,_) :-
    nl,!.

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
    finalSTATS(ATK, DEF, INT, MAXHP),
    currentHP(CurrentHP),
    ATK1 is round(ATK + 0.00000001), DEF1 is round(DEF + 0.00000001), INT1 is round(INT + 0.00000001), MAXHP1 is round(MAXHP + 0.00000001), CurrentHP1 is round(CurrentHP + 0.00000001),
    write('Status anda'), nl,
    write('Job      : '), write(Job), nl,
    write('Level    : '), write(Level), nl,
    write('Exp      : '), write(XP),write('/'),write(XP2), nl,
    write('HP       : '), write(CurrentHP1),write('/'),write(MAXHP1), nl,
    write('ATK      : '), write(ATK1), nl,
    write('DEF      : '), write(DEF1), nl,
    write('INT      : '), write(INT1), nl.

%% QUESTO
%% ==========================================================================================================

questto :-
    pos(player, 33, 16),
    questKomplito,
    questCounter(X),
    wr('Apakah Anda ingin menerima quest?'), 
    wr('1. yes'),
    wr('2. no'),
    write('>> '),
    read(Z),
    questOption(X,Z),!.


quest :- 
    adaQuest(true),
    questFunc(X,Y,Z), nl, X1 is X, Y1 is Y, Z1 is Z,
    (
        X < 0 -> X1 is 0;
        Y < 0 -> Y1 is 0;
        Z < 0 -> Z1 is 0;
        true
    ),
    write('>> Membunuh '), write(X1), write(' Goblin, '), write(Y1), write(' Slime, '), write(Z1), write(' Wolf.'),nl,!.
    
quest :-
    level(X),
    X > 49, nl,
    \+ isDragonDead(true),
    wr('>> Membunuh Naga').


questOption(1,1) :-
    retractall(questFunc(_,_,_)),
    X is 3,
    Y is 0,
    Z is 0,
    asserta(questFunc(X, Y, Z)),
    nl,
    wr('Tanpa Issei sadari, ia telah masuk ke ruang terdistori....'),
    wr('"Hah? Ada apa ini!"'),
    wr('*Clap* *Clap* *Clap*'),
    wr('[Selamat, kamu berhasil menemukanku]'),
    wr('Issei berbalik badan dan melihat Dewi itu lagi'),
    wr('[Apa yang kamu inginkan?]'),
    wr('"Sudah kusebutkan sebelumnya, kuharap kau masih ingat"'),
    wr('[Ara~ara mochiron desu~]'),
    wr('[Akan ku kabulkan setelah 3 goblin yang mengganggu itu]'),
    wr('Anda mendapatkan quest baru: Membunuh 3 goblin'),
    retractall(adaQuest(_)), asserta(adaQuest(true)), nl,!, true.

questOption(2,1) :-
    retractall(questFunc(_,_,_)),
    X is 3,
    Y is 3,
    Z is 3,
    asserta(questFunc(X, Y, Z)),
    nl,
    wr('Telah masuk ke ruang terdistori....'),
    wr('"Aku sudah selesai"'),
    wr('[Oh]'),
    wr('"Bagaimana dengan permintaanku?"'),
    wr('[Kau lihat bagaimana slime slime itu bergerak? Mereka sungguh menjijikan.]'),
    wr('Belum lagi anjing-anjing bodoh yang selalu berteriak di malam hari, sungguh menyebalkan!'),
    wr('Hey, ingat goblin yang kemarin kau bunuh? Sepertinya kakak dan teman-temannya ingin balas dendam. Singkirkan mereka semua]'),
    wr('"Haa baiklah baiklah akan kulakukan.."'),
    wr('Anda mendapatkan quest baru: Membunuh 3 goblin, 3 wolf dan 3 slime'), 
    retractall(adaQuest(_)), asserta(adaQuest(true)), nl,!, true.

questOption(3,1) :-
    retractall(questFunc(_,_,_)),
    X is 5,
    Y is 0,
    Z is 6,
    asserta(questFunc(X, Y, Z)),
    nl,
    wr('Memasuki Portal Dimensi lain....'),
    wr('[Selamat, kau telah menyelesaikan misi kedua]'),
    wr('*Clap* *Clap* *Clap*'),
    wr('[Sekarang kau akan menghadapi tahap selanjutnya.]'),
    wr('"APA ITU?!!!"'),
    wr('[Kau akan menghadapi banyak cobaan]'),
    wr('"What?!!, Cobaan apaa?!!"'),
    wr('[Dragon sudah terbangun dari tidur panjangnyaa]'),
    wr('[Dia akan menghancurkan dunia ini. Hanya kau yang dapat mengalahkannya]'),
    wr('[Tapi sebelum itu, tolong carikan aku 5 ekor goblin dan 6 ekor srigala.]'),
    wr('"Buat Apa itu semua?"'),
    wr('[Nanti kau akan tau pada saatnya]'),
    wr('Mengirim Issei kembali ke Isekai....'),
    wr('"AAARRGHH!!!"'),
    wr('Anda mendapatkan quest : Membunuh 5 Goblin, dan 6 wolf'), 
    retractall(adaQuest(_)), asserta(adaQuest(true)), nl,!, true.

questOption(4,1) :-
    retractall(questFunc(_,_,_)),
    X is 15,
    Y is 0,
    Z is 0,
    asserta(questFunc(X, Y, Z)),
    nl,
    wr('Aku keluaaaar....'),
    wr('"Aku sudah selesai"'),
    wr('[Oh]'),
    wr('"Bagaimana dengan permintaanku?"'),
    wr('[Oh tidak, kalungku telah dicuri.]'),
    wr('Huft... apa lagi sekarang -_-#'),
    wr('Hey, ingat kakak dan teman-teman goblin yang 3 bulan lalu kau bunuh? Sepertinya kakek merekalah yang telah mencurinya.]'),
    wr('"Haa baiklah baiklah akan kuhabisi mereka.."'),
    wr('Anda mendapatkan quest baru: Membunuh 15 goblin (saja)'), 
    retractall(adaQuest(_)), asserta(adaQuest(true)), nl,!, true.

questOption(5,1) :-
    retractall(questFunc(_,_,_)),
    X is 5,
    Y is 5,
    Z is 10,
    asserta(questFunc(X, Y, Z)),
    nl,
    wr('5 tahun kemudian...'),
    wr('"Sampai kapan aku harus menunggu?"'),
    wr('[Oh tidak, kalungku dicuri(lagi)]'),
    wr('"ノ( º _ ºノ)"'),
    wr('[Tenang, itu hanya kalung palsu.]'),
    wr('"（╯°□°）╯︵( .o.)"'),
    wr('[Baiklah, ini permintaan ku yang terakhir, Habisi 5 goblin, 5 slime, dan 10 wolf.]'),
    wr('"_-_"'),
    wr('[Sebagai imbalannya, kamu bebas meminta dan melakukan apapun kepadaku...]'),
    wr('"Σ(O_O；) (aku benci pikiranku)"'),
    wr('Anda mendapatkan quest baru (dan terakhir): Membunuh 5 goblin, 10 wolf dan 5 slime'), 
    retractall(adaQuest(_)), asserta(adaQuest(true)), nl,!, true.


questOption(_,1) :-
    wr('Quest belum tersedia lagi'), !, true.

questOption(_,2) :-
    wr('Anda menolak menerima quest!'),!, true.

questOption(_,_) :-
    wr('Input Salah!'), !, questto.

questKomplito :-
    adaQuest(true),
    questFunc(Goblin,Slime,Wolf),
    Goblin =< 0,
    Slime =< 0,
    Wolf =< 0,
    questCounter(X),
    X1 is X + 1,
    retractall(questCounter(_)),
    asserta(questCounter(X1)),
    level(OldLevel),
    ekspi(EXP),
    gold(GOLD),
    NEWEXP is 1000*X,
    NEWGOLD is 10000*X,
    EXP1 is EXP + NEWEXP,
    GOLD1 is GOLD + NEWGOLD,
    retractall(ekspi(_)),
    asserta(ekspi(EXP1)),
    level(NewLevel),
    fungsiRefreshDarah(OldLevel,NewLevel),
    retractall(gold(_)),
    asserta(gold(GOLD1)),
    retractall(adaQuest(_)),
    asserta(adaQuest(false)), nl,
    wr('QUEST SELESAI!!'),
    write('Anda mendapatkan: '), write(NEWEXP), write(' XP dan '),write(NEWGOLD), wr(' Gold'),
    !.

questKomplito :-
    adaQuest(true),
    wr('Anda belum menyelesaikan quest sebelumnya!'),nl,nl,!,fail.

questKomplito :-
    true.



%% Rule Tambahan
%% ==========================================================================================================
wr(Line) :-
    write(Line),nl.

help :-
    notInBattle(true),
    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
    write('%                                 Genshin Asik                                    %'), nl,
    write('% 1. start     : untuk memulai petualanganmu                                      %'), nl,
    write('% 2. map       : untuk menampilkan map                                            %'), nl,
    write('% 3. ascend    : ascencion untuk mengganti job anda (Minimal LVL 3)               %'), nl,
    write('% 4. status    : menampilkan kondisimu terkini                                    %'), nl,
    write('% 5. w         : gerak ke utara 1 langkah                                         %'), nl,
    write('% 6. s         : gerak ke selatan 1 langkah                                       %'), nl,
    write('% 7. d         : gerak ke ke timur 1 langkah                                      %'), nl,
    write('% 8. a         : gerak ke barat 1 langkah                                         %'), nl,
    write('% 9. store     : Membeli barang saat berada di toko \'S\'                           %'), nl,
    write('% 10.inventory : Membuka tas                                                      %'), nl,
    write('% 11.help      : menampilkan segala bantuan                                       %'), nl,
    write('% 12.save      : menyimpan state game                                             %'), nl,
    write('% 13.load      : memuat ulang state game yang terakhir                            %'), nl,
    write('% 14.quest     : melihat quest yang sedang berlangsung                            %'), nl,
    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,!.
    % nl,
    % nl,
    % write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
    % write('%                       Cara Buang Item dan Menggunakan Item                      %'), nl,
    % write('% 1. Buka inventory                                                               %'), nl,
    % write('% 2. 1 : Buang Item                                                               %'), nl,
    % write('% 3. 2 : Gunakan Item                                                             %'), nl,
    % write('% 4. Setelah memilih, masukan nama item sepersis mungkin                          %'), nl,
    % write('% dengan memerhatikan huruf kapital dan diapit tanda \'. Contoh: \'Wooden Sword\'    %'), nl,
    % write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,!.
    


help :-
    \+ notInBattle(true),
    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
    write('%                               CARA BERTARUNG                                    %'), nl,
    write('% 1. attack    : menyerang musuh                                                  %'), nl,
    write('% 2. usepotion : menggunakan potion                                               %'), nl,
    write('% 3. run       : Melarikan diri dari pertarungan, ada kemungkinan gagal           %'), nl,
    write('% 4. status    : menampilkan kondisimu terkini                                    %'), nl,
    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,!.
    


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
    wr('                                 -shmmy '),
    wr('Joestar Family Secret Move: Nigerundayoo!!'),nl,!.



gambar(wolf) :-                                       
    wr('            `sd/                                  '),
    wr('            ody/:`                                '),
    wr('          `-hNs:...``                             '),
    wr('          +ydh+-``-::.`                           '),
    wr('           .-..` ` ``:``                          '),
    wr('            .`   ```-+o-.`                        '),
    wr('            ``    `./ydso/-`                      '),
    wr('          ..```.```.+mmNmys+-                     '),
    wr('          :-```--..--smmmddy/.                    '),
    wr('          .:.`.-/:.`-/shmMMo                      '),
    wr('          `/.`.:od+.-:oohmNMh:                    '),
    wr('           .---:oh:--:oshmmNNNNho//:.             '),
    wr('           .-..-/s:--/ydmNNNmmNMNMMMNmmy/`        '),
    wr('           --....:--/ohdmmNNNNNNNMNMNNmdNN-       '),
    wr('           ./--:o/:+yhhysymNNNNmNNNNNNmmNNm.      '),
    wr('           --::/--.``..-+ymmNNNddmdNNmmmNNNd      '),
    wr('           -.--...````-:/sNdhmyoyhhmNmmmmmmM:     '),
    wr('           .-.`..`````.:oydy+y//+sohds+ohmmN+     '),
    wr('            -``..``````.oydo--:--//+.`-ohmmN+     '),
    wr('            ..`.-..```.-+sh/.```-:+-``./ydmN-     '),
    wr('             ..-+o:--..:+ys:...-:++.``.:sdmN      '),
    wr('             `..:hds::--+yo/:--/yms.```:oymy      '),
    wr('              ..:h-.:/:-/syoosdm+..+.`.:oym/      '),
    wr('              ../d    -:/ymdmmy` `.:o:`-/yd.      '),
    wr('              `.+o    `.+hyhdm`  --:/ys:-+h/      '),
    wr('              `-s.    `.o+:syd` `--/ssyo::oh/     '),
    wr('              `-o`    ``+:./oh/ `::shhys..:+y.    '),
    wr('              `-o     ..o:-:/o+  ysydmd`  -:s.    '),
    wr('              `:+     ..++://.   :mNNd.   .:o`    '),
    wr('             `./o    `.:/o/.      oNy.``.--/o     '),
    wr('           ./+osy:::::-/oo/://:::::+:::o+/s/+.`   '),
    wr('          .-:////////::/+/-----....````````       '),
    wr('                     ```                          '),!.



save :-
    notInBattle(true),
    write('Saving Data ...'),
	open('saveData.pl', write, Stream),
    write(Stream, 'loadstart :-\n'),

    started(Started),
    write(Stream, 'asserta(started('),
    write(Stream, Started),
    write(Stream, ')),\n'),


    jobFound(JobFound),
    write(Stream, 'asserta(jobFound('),
    write(Stream, JobFound),
    write(Stream, ')),\n'),

    pos(player, X, Y),
    write(Stream, 'asserta(pos(player,'),
    write(Stream, X),
    write(Stream, ','),
    write(Stream, Y),
    write(Stream, ')),\n'),

    job(A),
    write(Stream, 'asserta(job('),
    write(Stream, A),
    write(Stream, ')),\n'),

    ekspi(B),
    write(Stream, 'asserta(ekspi('),
    write(Stream, B),
    write(Stream, ')),\n'),

    gold(C),
    write(Stream, 'asserta(gold('),
    write(Stream, C),
    write(Stream, ')),\n'),

    currentHP(D),
    write(Stream, 'asserta(currentHP('),
    write(Stream, D),
    write(Stream, ')),\n'),

    questCounter(E),
    write(Stream, 'asserta(questCounter('),
    write(Stream, E),
    write(Stream, ')),\n'),

    notInBattle(F),
    write(Stream, 'asserta(notInBattle('),
    write(Stream, F),
    write(Stream, ')),\n'),

    adaQuest(Questttt),
    write(Stream, 'asserta(adaQuest('),
    write(Stream, Questttt),
    write(Stream, ')),\n'),


    questFunc(X1, Y1, Z1),
    write(Stream, 'asserta(questFunc('),
    write(Stream, X1),
    write(Stream, ','),
    write(Stream, Y1),
    write(Stream, ','),
    write(Stream, Z1),
    write(Stream, ')),\n'),


    equipedWeap(X2, Y2, Z2, W2),
    write(Stream, 'asserta(equipedWeap('),
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
    write(Stream, ')),\n'),

    equipedArmor(X3, Y3, Z3, W3),
    write(Stream, 'asserta(equipedArmor('),
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
    write(Stream, ')),\n'),

    equipedHead(X4, Y4, Z4, W4),
    write(Stream, 'asserta(equipedHead('),
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
    write(Stream, ')),\n'),

    equipedBoots(X5, Y5, Z5, W5),
    write(Stream, 'asserta(equipedBoots('),
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
    write(Stream, ')),\n'),

    equipedAcc(X6, Y6, Z6, W6),
    write(Stream, 'asserta(equipedAcc('),
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
    write(Stream, ')),\n'),

    write(Stream, 'asserta(bag(['),
    bag([H|T]),
    write(Stream, '\''),
    write(Stream, H),
    write(Stream, '\''),
 	close(Stream),
    tulisBag(T),
    tulisAkhir, nl,
    wr('GAME SUCCESFULLY SAVED').

load :-
    retractall(started(_)),
    retract(pos(player,_,_)),
    retractall(job(_)),
    retractall(ekspi(_)),
    retractall(gold(_)),
    retractall(currentHP(_)),
    retractall(questCounter(_)),
    retractall(notInBattle(_)),
    retractall(adaQuest(_)),
    retractall(questFunc(_,_,_)),
    retractall(jobFound(_)),
    retractall(bag(_)),
    retractall(equipedWeap(_,_,_,_)),
    retractall(equipedArmor(_,_,_,_)),
    retractall(equipedHead(_,_,_,_)),
    retractall(equipedBoots(_,_,_,_)),
    retractall(equipedAcc(_,_,_,_)),

 	consult('saveData.pl'),
    wr('Loading Data ...'),
    loadstart,
    wr('Game succefully loaded').

tulisBag([]).
tulisBag([H|T]) :-
    open('saveData.pl', append, Stream),
    write(Stream, ','),
    write(Stream, '\''),
    write(Stream, H),
    write(Stream, '\''),
    close(Stream),
    tulisBag(T).
    
tulisAkhir :-
    open('saveData.pl', append, Stream),
    write(Stream, '])).\n'),
    close(Stream).

% Sumber
% StackOverFlow
% Prolog Documentations
% https://www.youtube.com/watch?v=Sglwxswyo4o&ab_channel=MeenakshiHooda
% https://youtu.be/SykxWpFwMGs
% https://www.youtube.com/watch?v=B7HGbMnNX0U