# Laborator 5 - STM

Pentru acest laborator vom folosi o librarie C++ ce implementeaza STM (Simple Transactional Memory): [cpp\_stm\_free](https://github.com/graninas/cpp_stm_free).

Cel mai usor este sa puneti [folderul](https://github.com/graninas/cpp_stm_free/tree/master/cpp_stm) in acelasi folder cu sursa voastra si sa compilati programul vostru impreuna cu fisierul `context.cpp`.
De exemplu daca sursa voastra se numeste `main.cpp` si in acelasi folder cu ea este folderul `stm`:

`g++ -std=c++17 -lpthread -o main stm/context.cpp main.cpp`

Pentru windows puteti sa adaugati fisierele in acelasi proiect cu sursa voastra (de exemplu daca folositi Visual Studio).

Libraria implementeaza conceptul de STM ca monada si are niste operatii de baza (pe care le puteti gasi si in [tutorial](https://gist.github.com/graninas/c7e0a603f3a22c7e85daa4599bf92525)).

Un exemplu de program care porneste 5 thread-uri care sa numere concurent pana la 100 este urmatorul:

```cpp
#include <thread>

#include "stm/stm.h"

using namespace std;
using namespace stm;

STML<int> incrementCounter(const TVar<int>& counter)
{
    STML<Unit> modified = modifyTVar<int>(counter, [](int i) { return i + 1; });

    // bind takes a STML<X> and a function X -> STML<Y> and gives you a STML<Y>
    // it's good when you need to process some things in order and you need the results
    // from previous computations.
    return bind<Unit, int>(
        modified,
        [&](const Unit&){ return readTVar<int>(counter); }
    );


    /* Or a variant using sequence, might be easier to read, but is not as flexible
    return sequence(
        modifyTVar<int>(counter, [](int x) { return x + 1; }),
        readTVar<int>(counter)
    );
    */
}

struct CounterRt {
    int thread;                // Thread number
    std::mutex& logLock;       // Mutex for logging
    Context& context;     // STM Context
    TVar<int> tCounter;   // Shared counter TVar
};
 
// Thread worker function
void counterWorker(const CounterRt& rt) {
   for (int i = 0; i < 50; ++i) {
       int counter = atomically(rt.context, incrementCounter(rt.tCounter));
       std::lock_guard g(rt.logLock);
       std::cout << "thread: [" << rt.thread << "] counter: " << counter << std::endl;
   }
}

// main function
int main() {
    Context ctx;
    TVar<int> tCounter = newTVarIO(ctx, 0);

    std::mutex logLock;

    std::vector<std::thread> threads;
    for (int i = 0; i < 5; ++i) {
        threads.push_back(std::thread(counterWorker, CounterRt {i, logLock, ctx, tCounter}));
    }

    for (auto& t: threads)
        t.join();

    return 0;
}
```

Vom simula in acest laborator lupta dintre un cavaler si un dragon folosind STM-uri. Daca doriti puteti folosi si alte 
librarii care simuleaza STM, fie in C++, fie in Java.

1. Povestea incepe cu un cavaler si un dragon. Cavalerul are o sabie cu rezistenta infinita, iar dragonul o cantitate 
   de viata. 

   Ei se intalnesc la momentul 0, iar de la acel moment cavalerul isi foloseste sabia 
   pentru a lovi dragonul. Pana cand dragonul este mort cavalerul va lua sabia si il va lovi pe dragon, fiecare lovitura
   decrementand intre 1 si 20 din cantitatea de viata a dragonului.

   Dragonul din secunda in secunda, cat timp este in viata, va tipa de furie.

   Simulati aceasta lupta folosind STM:
    * Atat cavalerul cat si dragonul vor fi un thread
    * Singura resursa pentru moment este starea dragonului 
    * Cand dragonul tipa trebuie sa afisati la standard error un mesaj.
    * Trebuie deasemenea sa tineti minte cat de mult din viata dragonului a decrementat cavalerul, pentru a afisa la finalul programului.
    * Sunteti liberi sa alegeti ce cantitate de viata avea original dragonul. Alegeti in asa fel incat lupta sa dureze intre 5 si 10 secunde. 
      In functie de calculatoarele voastre asta inseamna intre 100.000 si 10.000.000.

   De exemplu daca dragonul are o cantitate de viata egala cu 30 lucrurile s-ar putea intampla astfel:
    * Cavalerul il loveste si ii decrementeaza 12 din cantitatea de viata 
    * Cavalerul il loveste si ii decrementeaza 9 din cantitatea de viata 
    * Poate thread-ul cavalerului este oprit temporar (din motive externe), si trece o secunda 
    * Dragonul tipa: "ROAAAAR"
    * Cavalerul il loveste si ii decrementeaza 17 din cantitatea de viata, dragonul moare.
    * In total cavalerul a decrementat din viata dragonului: 38.

2. Se alatura cavalerului si un arcas. Acesta trage in dragon incontinuu (presupunem ca are o infinitate de sageti), fiecare lovitura
   decrementand intre 2 si 5 din cantitatea de viata a dragonului. Contorizati si cata cantitate de viata decrementeaza arcasul in aceasta situatie.

3. Dragonul nu mai tipa din secunda in secunda, in schimb sufla flacari. Incepand cu momentul 0 dragonul va functiona in momentul urmator:
    * Isi va conserva energia pentru exact o secunda.
    * Daca inca este in viata va incepe sa sufle flacari pentru un interval de timp aleator intre 0.5 si 1 secunda.
    * In timp ce sufla flacari nici cavalerul, nici arcasul nu pot sa il atace.
    * Dupa ce termina de suflat o ia de la capat (evident pana cand este mort).

   Scrieti la standard error cand dragonul incepe sa sufle flacari/termina de suflat. Evident veti avea o noua resursa in aceasta problema:
    * Sufla/nu sufla flacari dragonul.

   Aici puteti folosi `stm::retry<Tip>()` cand cavalerul sau arcasul vor sa loveasca dragonul dar el sufla flacari. Cand se intoarce rezultatul acestei functii dintr-un bloc atomic, se va reincerca executarea acestuia 
   automat. 

   De aici in jos urmeaza tema BONUS pentru 10 puncte, 5 puncte la fiecare exercitiu (Deadline ~~30 Martie~~ 06 Aprilie 23:59):

4. Cand dragonul sufla flacari exista o sansa sa ii arda sabia cavalerului. Mai exact, apare o resursa noua ce trebuie sincronizata: sabia.
   Daca cavalerul nu are sabia bine prinsa (nu are resursa sabie inca), dragonul o poate lua si ii va da foc. 
   Dupa ce dragonul termina de suflat (si sabia nu mai e incinsa), este inca totusi prea fierbinte pentru ca cavalerul 
   sa o foloseasca, si acesta o va pune la racit. Racirea poate dura intre 0.5 si 1.5 secunde, dar dragonul nu ii mai poate 
   da foc.

5. Se alatura cavalerului si arcasului un magician. Pe deasupra arcasul realizeaza ca nu are o infinitate de sageti la dispozitie, are numai 100.

    Aceasta are o resursa numita mana, cu capacitate 6.
    De fiecare data cand rosteste o vraja ii scade cu 1 mana. Cand ajunge la 0 mana trebuie sa se concentreze pentru un timp aleator intre 1 si 2 secunde, timp 
    in care isi va recupera toata mana (de la 0 va ajunge la 6).

    El cunoaste 2 vraji:
    - Magic bolt: Sa o rosteasca dureaza intre 0.1 si 0.2 secunde aleator si loveste dragonul pentru 5 000 - 10 000 de unitati de viata.
    - Create arrows: Sa o rosteasca dureaza intre 0.3 si 0.5 secunde aleator si creeaza 100 de sageti pentru arcas.

    Magicianul va rosti incontinuu `Magic bolt` pana cand este notificat de arcas ca acesta nu mai are sageti. Cand este notificat, va rosti in schimb `Create arrows`. 
    Daca este notificat in timp ce isi recupereaza mana sau rosteste `Magic bolt` atunci va crea sagetile dupa ce termina cu actiunea curenta.

    Arcasul va notifica magicianul ca are nevoie de sageti doar cand ajunge la 0 sageti.

    Aici veti avea la dispozitie o resursa in plus: Daca arcasul are/nu are nevoie de sageti.

    Contorizati si cat de mult din cantitatea de viata a dragonului a luat magicianul.

