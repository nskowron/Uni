# Sprawozdanie - Technologie Sieciowe, Lista 1

## Komunikacja z zewnetrznym serwerem

Pingowalam serwer youtube'a, co, jak widac ponizej, dalo sie wychwycic WireSharkiem.
Dodatkowo, odpalilam jednoczesnie w przegladarce strone YouTube i, zgodnie z oczekiwniami,
dalo sie zaobserwowac komunikacje z tym samym adresem uzywajaca protokolu TCP.

![ping yt](image-1.png)
![tcp yt](image-2.png)

Ponizej wyznaczalam liczbe wezlow do serwera youtube za pomoca pinga i traceroute'a.
Odleglosc z mojej sieci wynosi 11 wezlow:

![ttl yt](image-3.png)
![trace yt](image-13.png)

## Rozne trasy

Jak widac ponizej - trasy przebyte przez pakiety moga byc rozne i miec rozne dlugosci,
w zaleznosci od - np. jak ponizej - ustawionego TTL.

![diff ttl](<Screenshot From 2025-03-13 12-23-03.png>)

## Fragmentacja pakietow i czas propagacji

Jak widac ponizej, YouTube i wiele innych serwerow blokuja pakiety, ktore wymagaja fragmentacji.

![alt text](image-5.png)

Spingowalam wiec kolege we wspolnej sieci lokalnej:

![big ping](<Screenshot From 2025-03-13 12-01-37.png>)
![small ping](image-6.png)

Jak widac, czas dojscia pakietow rozni sie znaczaco w wypadku wyslania standardowego 56B
pakietu a 64kB. Jest to niewatpliwie zwiazane z koniecznoscia czekania na wszystkie
fragmenty oraz zlozenia ich spowrotem.

Aby sprawdzic, czy pakiet rzeczywiscie wymaga fragmentacji, wystarczy uzyc odpowiednich flag 
w programach ping lub traceroute, co widac w nastepnej sekcji - lub uzyc programu WireShark:

![frag yt](image-9.png)

## Najwiekszy niefragmentowany pakiet

Metoda bisekcji sprawdzilam, ze najwiekszy niefragmentowany pakiet to 1500B (z ramka).

![frag](image-8.png)

## Komunikacja w LAN

Jak widac na jednym z poprzednich screenshotow - zgodnie z oczekiwaniami pakiet  
do urzadzenia w LAN nie wyjdzie poza LAN:

![small ping](image-6.png)

Dodatkowo, uzywajac narzedzia nc, mozna wysylac do siebie wiadomosci, a nastepnie podsluchiwac 
w WireSharku.

![hejo](<Screenshot From 2025-03-13 11-39-53.png>)

Natomiast WireShark nie podsluchuje takich rozmow pomiedzy innymi dwoma 
urzadzeniami w tej samej sieci lokalnej.

## Srednica internetu

Mozemy probowac eksperymentalnie wyznaczac najdluzsza sciezke do serwera, natomiast musimy 
brac pod uwage, ze tez znajdujemy sie w jednym z wezlow sieci, i to jednym z lepiej skomunikowanych, 
wiec najwieksza odleglosc do nas (i od nas) na pewno i tak bedzie mniejsza niz faktyczna 
srednica internetu.

Najdluzsza sciezka, jaka udalo mi sie znalezc to 24 do sydney.edu.au:

![sydney](image-10.png)

## Serwery chmurowe

![cloud](image-11.png) ![alt text](image-12.png)

Jak widac, w przypadku serwerow Google i Microsoft, dlugosc trasy nie odbiega za bardzo 
od standardowych 10 wezlow. Nie dziwi nas to, poniewaz serwery takich uslug musza byc 
bardzo dobrze skomunikowane.
