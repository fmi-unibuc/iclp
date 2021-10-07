# Laborator 2 + 3

1. Incrementarea unui contor de pe mai multe threaduri.

Daca rulam urmatorul cod:
```java
public class Counter {
    static int counter = 0;
    static final int ITERATIONS = 1_000_000;

    public static void main(String[] args) throws InterruptedException {
        Thread t1 = new Thread(() -> {
            for (int i = 0; i < ITERATIONS; ++i)
                ++counter;
        });

        Thread t2 = new Thread(() -> {
            for (int i = 0; i < ITERATIONS; ++i)
                ++counter;
        });

        t1.start();
        t2.start();

        t1.join();
        t2.join();

        System.out.println("Counted " + counter);
    }
}
```

O sa vedem ca se afiseaza altceva decat **2 000 000**. 

Corectati exemplul folosind un obiect static declarat in clasa Counter pentru sincronizare. 

2. Scrieti un program care porneste 2 thread-uri *Producator* si *Consumator*. 

Fara sa folositi vreo primitiva de sincronizare, incercati, folosind `java.util.ArrayDeque` sa simulati o coada. 

- *Producator* va adauga 1\_000\_000 de elemente in spatele cozii. (Puteti folosi `addLast`)
- *Consumator* va incerca sa scoata toate elementele din coada. (Puteti folosi `pollFirst`)

Ce observati?

Sincronizati operatiile peste `java.util.ArrayDeque` pentru ca programul sa lucreze corect.

3. Adaptati programul de la 2. astfel incat *Producator* sa stie cand *Consumator* a terminat cu coada.
Adaugati deasemenea un mic timp de asteptare inainte ca *Consumator* sa isi inceapa treaba.
Puteti folosi `Thread.sleep(millisecunde)` pentru asta.

O solutie ineficienta ar fi 
```java
while (queue.size() > 0) {}
```

Daca va uitati in task manager (windows) sau top (linux) veti observa ca unul din procesoare este permanent la 100%, 
doar de la acea verificare in bucla.

Folosind metodele `wait` si `notify` pe obiectul de sincronizare pentru a nu suprasolicita procesorul in asteptarea lui `Consumator`.

4. Pornind de la 3, reduceti numarul de elemente din coada semnificativ (la 10-20 ar fi bine) si adaugati o intarziere random intre 
oricare doua elemente adaugate in coada. (De exemplu folosind `Thread.sleep((long)(Math.random() * 2000))` face thread-ul curent sa astepte pentru 
o perioada aleatorie intre 0 si 2 secunde)

Atentie! Nu puneti sleep in interiorul unui block `synchronized`. Nu este niciodata o idee buna, intrucat niciun alt thread nu va mai putea sa foloseasca 
obiectul sincronizat in acea perioada.

Ce observati? 

Corectati probleme de utilizare ale procesorului ca si la 3.


Observatii: Aveti documentatie pentru `wait` si `notify` la [Java 7 - API](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#wait())

5. Extindeti codul de la 4 in asa fel incat sa puteti avea mai multi producatori si sau consumatori.

Pana acum probabil ati stiut exact cate elemente trebuie consumatorul sa scoata din coada. 
Deoarece pot fi mai multi consumatori, si fiecare poate scoate un numar variabil de elemente va trebui sa anuntati cumva consumatorii ca niciun producator 
nu va mai adauga elemente in coada.

6. Inlocuiti `java.util.ArrayDeque` cu o alternativa thread-safe. Aveti mai multe optiuni, insa cea mai simpla este `java.util.concurrent.ArrayBlockingQueue`.

Observati ce anume va simplifica aceasta modificare.
