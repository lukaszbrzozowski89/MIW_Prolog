:- dynamic at/2.
:- dynamic hasKey/2.
:- dynamic hasFuel/2.
:- dynamic hasKeyAndFuel/2.

description(dom,
  ' Jesteś w domu.. Przed Tobą ścieżka. ').
description(sciezka,
  ' Jesteś na ścieżce, przed Tobą las Uważaj na siebie. ').
description(las(_),
  ' Jesteś w lesie, możesz iśc w każdą stronę Tylko się nie zgub. ').
description(jezioro,
  ' Spadłeś do wody. ').
description(wawoz,
  ' Jesteś w pięknym wąwozie. Po lewej piękne widoki, prosto jest jakaś ciemna droga... ').
description(przepasc,
  ' Spadłeś w przepaść.. ehh').
description(droga,
  ' Znalazłeś się na drodze, przed Tobą samochód, po prawej ładna chatka, a po lewej coś się rusza za kamieniem. ').
description(chatka,
  ' Jesteś w chatce. Chyba tego się nie spodziewałeś... ').
description(kamien,
  ' Jesteś przy kamieniu, mozesz tylko wrócić... ').
description(auto,
  ' Jesteś w samochodzie, potrzebujesz kluczyków aby go uruchomić. ').
description(auto1,
  ' Samochód odpalony, musisz uciekać. ' ).
description(auto2,
  ' Uciekasz...  ').

plecak(['kluczyki', ' paliwo']).
wypisz_liste([H|T]) :- write('Masz '), write(H), wypisz_liste(T),nl.
wypisz_klucze([H|_]) :- write('Masz '), write(H),nl.
wypisz_paliwo([_|T]) :- write('Masz '), write(T),nl.
klucze:- plecak(X), wypisz_klucze(X).
paliwo:- plecak(X), wypisz_paliwo(X).
rzeczy :- plecak(X), wypisz_liste(X).

items:-
    at(you, Loc),
    hasKey(you, Loc),
    nl,
    klucze,
    nl.

items:-
    at(you, Loc),
    hasKeyAndFuel(you, Loc),
    nl,
    rzeczy,  
    nl.

items:-
    at(you, Loc),
    hasFuel(you, Loc),
    nl,
    paliwo,
    nl.

items:-
    nl,
    write(' Masz pusty plecak. '), nl.  

report :-
  at(you,X),description(X,Y), write(Y), nl, items, nl.


connect(sciezka,back,home).
connect(las(0),back,sciezka).
connect(auto,back,droga).
connect(auto1,back,auto).
connect(auto2,back,auto1).
connect(droga,back,wawoz).
connect(kamien, back, droga).

connect(dom, forward, sciezka).
connect(sciezka, forward, las(0)).
connect(las(0), right, jezioro).
connect(las(0), left, las(1)).
connect(las(1), right, las(2)).
connect(las(1), left, las(3)).
connect(las(1), forward, las(0)).
connect(las(2), forward, las(1)).
connect(las(2), right, las(3)).
connect(las(3), forward, wawoz).
connect(wawoz, left, przepasc).
connect(wawoz, forward, droga).
connect(droga, right, chatka).
connect(droga, left, kamien).
connect(droga, forward, auto).


move(forward) :-
  at(you,auto),
  hasKey(you, auto),
  write(' Otwierasz auto bez kluczyków, ktoś Cie dogonił. '),
  nl,
  retract(at(you,auto)),
  assert(at(you,done)),
  !.

move(forward) :-
  at(you,auto1),
  hasFuel(you, auto1),
  write(' Ruszyłeś bez paliwa, zombiak Cie dorwał. '),
  nl,
  retract(at(you,auto1)),
  assert(at(you,done)),
  !.

move(Dir) :-
  at(you,Loc),
  connect(Loc,Dir,Next),
  write(Dir),
  retract(at(you,Loc)),
  assert(at(you,Next)),
  report,
  !.

move(pickup) :-
  at(you,Loc),
  kluczyki(Loc),
  report,
  !.

move(take) :-
  at(you,Loc),
  paliwo(Loc),
  report,
  !.

move(open) :-
  at(you,auto),
  hasKey(you, auto),
  write('-'),write(open),write('--'),
  nl,
  write(' Otworzyłeś samochód. '),
  nl, nl, 
  retract(at(you,auto)),
  assert(at(you,auto1)),
  report,
  !.

move(use) :-
  at(you,auto1),
  hasFuel(you, auto1),
  write('-'),write(use),write('--'),
  nl, 
  write(' Na szczescie wziałeś paliwo. '),
  nl,nl,
  retract(at(you,auto1)),
  assert(at(you,auto2)),
  report,
  !.

move(_) :-
  write(' Nie możesz tam pójśc. '), nl,
  report.


dead :-
  at(dead,Loc),
  at(you,Loc),
  write(' Zabiłeś się. '),
  retract(at(you,Loc)),
  assert(at(you,done)),
  !.

dead.

zombie :-
  at(zombie,Loc),
  at(you,Loc),
    nl,
  write('Zombie Cie dorwał.'),
  nl,
  write('Giniesz!.\n'),
  retract(at(you,Loc)),
  assert(at(you,done)),
  !.

zombie.

endgame :-
  at(endgame,Loc),
  at(you,Loc),
  write(' WOOOOW, uciekłeś zombiakom!'),
  nl,
  write(' Brawo, wygrałeś!'),
  nl,
  retract(at(you,Loc)),
  assert(at(you,done)),
  !.

endgame.

kluczyki(Loc) :-
  at(kluczyki,Loc),
  at(you,Loc),
  retract(hasKey(you, default)),
  assert(hasKey(you, _)),
  write(' Podnosisz jakieś kluczyki!'),
  nl,
  !.

kluczyki(_) :-
  write(' Tu nie ma kluczyków'),
   nl,
  !.  

paliwo(Loc) :-
  at(paliwo,Loc),
  at(you,Loc),
  retract(hasFuel(you, default)),
  assert(hasFuel(you, _)),
  write(' Znalazłeś paliwo'),
  !.

paliwo(_) :-
  write(' Tu nie ma paliwa'),
    nl,
  !. 

hasKey(you, default).

hasFuel(you, default).

hasKeyAndFuel(you, default).


jezioro :-
  at(you,jezioro),
  write(' Topisz się.'),
  nl,
  retract(at(you,jezioro)),
  assert(at(you,done)),
  !.

jezioro.

chatka :-
  at(you,chatka),
  write(' Hahahah.'),
  nl,
  retract(at(you,chatka)),
  assert(at(you,endgame)),
  !.

chatka.

main :- 
  at(you,done),
  write(' Dzięki za grę!'),
   nl,
  !.

main :-
  write(' Czekam na kolejny ruch... '),
  read(Move),
  call(move(Move)),
  zombie,
  endgame,
  jezioro,
  chatka,
  dead,
  main.

go :-
  retractall(at(_,_)),
  assert(at(you,dom)),
  assert(at(zombie,las(2))),
  assert(at(zombie,chatka)),
  assert(at(dead,jezioro)),
  assert(at(dead,przepasc)),
  assert(at(kluczyki,las(3))),
  assert(at(paliwo,kamien)),
  assert(at(endgame,auto2)),
  write(' Uniknij śmierci i wygraj'),
  nl,
  write(' Możesz się poruszać po angielsku, bo pierwsze litery się nie pwowtarzają '),
  nl,
  write(' (l)eft, (r)ight, (f)orward, (b)ack, (o)pen, (p)ickup, (u)se, (t)ake'),
  nl,
  write(' Powodzenia! '),
  nl,
  nl,
  report,
  main.