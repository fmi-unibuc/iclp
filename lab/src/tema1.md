# Tema 1 - Deadline 30 Martie 2020 - 23:59

Simulati o cofetarie care produce torturi si apoi le livreaza.

O comanda este de forma: NUME X Y Z T unde 
* __NUME__ este un sir de litere mici/mari ale alfabetului englez si/sau simboluri -_+.
* __X__ reprezinta durata de preparare a blatului in milisecunde.
* __Y__ reprezinta durata de preparare a cremei in milisecunde.
* __Z__ reprezinta durata de preparare a decoratiunilor in milisecunde.
* __T__ reprezinta durata de livrare a tortului (cat timp ii ia unei persoane pentru a livra tortul si a se intoarce inapoi la cofetarie).

La aceasta cofetarie lucreaza:
* 1 `Responsabil telefonic`, care preia comenzile pentru torturi pe masura ce vin si le trimite catre `Manageri`.
  O comanda va fi trimisa fie catre un `Manager` disponibil (care nu este ocupat), fie va fi pus intr-o coada de comenzi.
  Coada de comenzi are capacitate maxim 4, si cand aceasta se umple, `Responsabilul telefonic` va trebui sa astepte ca un
  `Manager` sa mai preia   din comenzi.
* 6 `Manageri`. La orice moment de timp un `Manager` fie se ocupa de o comanda, fie asteapta sa preia o comanda sau asteapta un `Livrator` sa fie disnpoibil.
  Dupa ce preia o comanda, acesta va distribui cei _3_ pasi din prepararea tortului, fiecare catre un cofetar disponibil.
  Va astepta ca cei 3 cofetari sa isi termine fiecare partea lui, fara sa se ocupa de vreo alta comanda intre timp, dupa care va lua tortul (presupunem ca in secunda in care toti cei 3
  cofetari si-au terminat treaba tortul este complet asamblat) si il va da catre un `Livrator` disponibil. 
  
  Daca nu exista niciun `Livrator` disponibil, acesta va astepta pana cand unul se va intoarce si ii va prelua tortul spre livrare.
  Din momentul in care un `Livrator` preia tortul de la `Manager`, acesta (`Managerul`) se poate ocupa de alta comanda.
* 15 `Cofetari`. La orice moment de timp un `Cofetar` fie asteapta, fie se ocupa de un pas din preparearea unui tort (blat/crema/decoratiuni).
  Cand primeste de la `Manager` de efectuat un anumit pas, se va concentra pe acel pas pana la terminare, dupa care va putea efectua alte instructiuni.
* 5 `Livratori`. Ei asteapta sa primeasca de la `Manageri` torturi pentru livrare, dupa care ii vor livra in timpul lor de livrare corespunzator.

Reguli de implementare:
* Timpii de preprare/livrare se vor simula folosind `Thread.sleep`
* Thread-ul principal va reprezenta `Responsabilul telefonic`, si va citi comenzile, cate una pe un rand de la standard input.
* Puteti folosi orice structura deja implementata pentru comunicarea dintre `Responsabilul telefonic` si `Manageri` care simuleaza corect cerinta.
* Cofetarii trebuiesc simulati folosind un `ThreadPoolExecutor`.
* Pentru comunicarea dintre `Manageri` si `Livratori` trebuie sa implementati voi detaliile de sincronizare folosind `wait`, `notify`, `ReentratLock`, `Condition` si asa mai departe.
* Managerii trebuie sa afiseze la standard err (`System.error`) atunci cand preiau comenzi si atunci cand le-au oferit spre livrare unui `Livrator.
* Livratorii trebuie sa afiseze la standard output (`System.out`) atunci cand au terminat de livrat o comanda.
* Respectand tot de mai sus puteti lua pana la 18 puncte pentru aceasta tema. Pentru nota maxima(20 de puncte), cei 15 `Cofetari` au functii specifice, iar manager-ul trebuie sa distribui fiecare din cei 3 pasi din prepararea tortului catre un alt tip de cofetar:
  * Primii 5 stiu doar sa prepare blatul
  * Urmatorii 5 stiu doar sa prepare crema
  * Ultimii 5 stiu doar sa prepare decoratiuni.

