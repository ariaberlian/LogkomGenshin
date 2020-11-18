
:- dynamic(job/1).
      


mulai :-
    write('Youkouso genshin sekai'), nl,
    write('Choose your character:'), nl,
    write('1. Swordsman'), nl,
    write('2. Archer'), nl,
    write('3. Wizard'), nl,
    read(Character),
    pengecekan(Character).

loves(aria,yudi).
    



pengecekan(1) :-
    write('Anda menjadi swordsman'),
    retractall(job(_)),
	asserta(job(swordsman)).
pengecekan(2) :-
    write('Anda menjadi archer'),
    retractall(job(_)),
    asserta(job(archer)).
pengecekan(3) :-
    write('Anda menjadi wizard'),
    retractall(job(_)),
	asserta(job(wizard)).