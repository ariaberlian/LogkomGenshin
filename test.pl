
:- dynamic(job/1).
:- dynamic(jobNotFound/1).
:- dynamic(jobFound/1).
:- dynamic(level/1).
% DYNAMIC disini adalah supaya bisa kita retract(hapus) dan assert(tambahin).
      

jobNotFound(true).

% jobFound(true) :-
%     jobNotFound(false).

% jobFound(false) :-
%     jobNotFound(true).

level(1).
% JOB = SWORDSMAN (ANGKA DIBAWAH Hanyalah Contoh, ganti sesuka mungkin)
% Jadi ceritanya baseATK/DEF/INT/HP itu stat awal yg keubah hanya dengan level
% ntr ada stat lain kyk ATK doang itu jadi buat multipler base atk kita sama misal equipment
baseATK(Y) :-
    level(X),
    Y is 100+(100*(X-1)*0.1),
    job(swordsman).
baseDEF(Y) :-
    level(X),
    Y is 100+(100*X*0.1),
    job(swordsman).
baseINT(Y) :-
    level(X),
    Y is 100+(100*X*0.1),
    job(swordsman).
baseHP(Y) :-
    level(X),
    Y is 100+(100*X*0.1),
    job(swordsman).


fungsi :-
    jobFound(true),
    write('Anda sudah memilih character').


mulai :-
    fungsi;
    jobNotFound(true),
    write('Youkouso genshin sekai'), nl,
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
    asserta(jobFound(true)).
pengecekan(2) :-
    write('Anda menjadi archer'),
    asserta(job(archer)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)).
pengecekan(3) :-
    write('Anda menjadi wizard'),
	asserta(job(wizard)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)).
pengecekan(4) :-
    write('Anda menjadi priest'),
	asserta(job(priest)),
    retractall(jobNotFound(_)),
    asserta(jobFound(true)).
pengecekan(X) :-
    X > 4,
    write('Tidak ada job tersebut').