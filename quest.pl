/*Quest merupakan misi yang harus diselesaikan oleh pemain, dan diambil dari tempat pengambilan Quest. */
/*Sebuah quest terdiri atas sebuah tripel bilangan (x,y,z), */
/*yang berarti untuk menyelesaikan quest tersebut pemain harus mengalahkan */
/*x buah slime, y buah goblin, dan z buah wolf. */
/*Apabila menyelesaikan sebuah quest, pemain menerima reward berupa tambahan EXP serta sejumlah gold. */
/*Dalam satu waktu, tidak boleh terdapat dua quest yang aktif, */
/*yang berarti pemain harus menyelesaikan sebuah quest terlebih dahulu sebelum dapat menerima quest yang baru.*/
/*Implementasi penentuan quest serta rewardnya dibebaskan, dan harus dijelaskan pada laporan.*/

:- dynamic(pos/3).
x(slime).
y(goblin).
z(wolf).

quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)
quest(1)

quest :-
	pos(player, 33, 16),
	write ('Selamat datang di QUEST!!!'),nl,
	write ('Quest ini memiliki 50 ronde yang akan terbuka sesuai dengan tingkat levelmu'), nl,
	write ('Pilih Quest yang ingin kamu selesaikan'), nl,
	write ('1. Ayah, mengapa aku tampan? (minimal player level 5)'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('1. Ayah, mengapa aku tampan?'), nl,
	write ('>>'),
	read (Chapter)	
	battle(
	
	

	write ('---SLIME---'), nl,
	battle(slime),
	write ('Selamat, kamu telah mengalahkan monster pertama, sekarang habisi monster kedua'), nl,
	write ('---GOBLIN---'), nl,
	battle(slime),
	write ('Selamat, kamu telah mengalahkan monster kedua, sekarang habisi monster terakhir'), nl,
	write ('---WOLF---'), nl,
	write ('Selamat kamu telah menyelesaikan quest kali ini.'), nl,
	write ('You get +'),
	
	
	