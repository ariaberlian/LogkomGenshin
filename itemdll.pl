:- dynamic(bag/1).

/* *** Item *** */
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
gold(300).

/* *** List rules *** */
concat([],L2,L2).
concat([H|T], L2, [H|L3]):-concat(T,L2,L3).

push(Element,Queue,Result):- concat(Queue,[Element],Result).

firstOut([_|T],T).
pop(Queue,Result):-firstOut(Queue,Result).

front([H|_],H).
back([H],H).
back([_|T],Result):-back(T,Result).

list_length([]     , 0 ).
list_length([_|T] , N ) :- list_length(T,N1) , N is N1+1.

lastOut([H|T], Result):-lastOutprev(T, Result, H).            
lastOutprev([], [], _).
lastOutprev([H1|T1], [H0|T0], H0):-lastOutprev(T1, T0, H1). 

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

jajan(M, P, Result) :-
    M >= P, Result is M-P.
jajan(M, P, _Result) :-
    M < P, write('Gold anda tidak mencukupi!'),nl.

/* *** Inventory *** */
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
    write('>>'),
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

/* *** Store *** */
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
 