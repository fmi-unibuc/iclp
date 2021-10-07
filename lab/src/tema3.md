# Tema 3

Simulati un cralwer folosind un event loop. Puteti folosi:
 * Javascript(si derivatele) traditional sau cu Async/Await 
 * Python cu async await 
 * Alte limbaje, dar folosind librarii pentru acest mecanism: de exemplu Async/Await in rust, sau libuv in C/C++.

Cu exceptia librariei care va ofera suport pentru event loop, si eventual al unor structuri de date (coada, map, hash table) nu aveti voie sa folositi 
librarii care sa implementeze functionalitatea crawler-ului.

 1. Crawler-ul va porni de la un url dat, va descarca pagina de la acel url, va scoate toate url-urile si va continua cu toate url-urile incarcate.

 2. Aveti grija sa nu crawlati de 2 ori acelasi url. 

 3. Nu trebuie sa descarcati o pagina in intregime pentru a cauta url-uri in ea. Cautati url-uri in paginile descarcate pe masura ce le trageti. 
    Nu mentineti toata pagina in memorie, in realitate orice url are maxim 2000 de caractere, deci va trebuie maxim 2000 de bytes per pagina tinuti minte.

4. Daca vreo pagina nu se incarca (din orice motiv) reincercati-o. Nu reincercati o pagina de mai mult de `X` ori. Faceti asta si daca ati reusit sa descarcati
deja cateva bucati din pagina si ati intampinat o problema dupa.

5. Folosind o coada, faceti in asa fel incat nu mai mult de `X` requesturi se fac in paralel.

6. Generalizati solutia de la `5.` pentru a garanta ca nu mai mult de `Z` requesturi se fac in paralel pentru acelasi domeniu.

Puteit alege voi `X`, `Y` si `Z`. De exemplu niste numere rezonabile sunt: 
 * `X = 3`
 * `Y = 20`
 * `Z = 4`
