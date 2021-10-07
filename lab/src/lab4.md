# Laborator 4

Pentru acest laborator vom folosi `java.util.concurrent.ExecutorService`.

1. Scrieti un program care citeste de la standard input o lista de numere (pana la aparitia lui 0) si raspunde pentru fiecare daca este prim sau nu si afiseaza 
la standard output (`System.out`).

Observatii: nu conteaza algoritmul folosit, puteti face si urmatoarea verificare:

```java
bool prim = true;
for (int i = 2; i < numar; ++i)
    if (numar % i == 0)
        prim = false;
```

2. Folosind un ThreadPoll (fie direct `java.util.concurrent.ThreadPoolExecutor` fie, mai simplu, `java.util.concurrent.Executors::newFixedThreadPool`) faceti in asa fel incat 
sa se verifice in paralel pentru 3 numere daca sunt prime sau nu.

3. Configurati thread-pool-ul pentru ca marimea cozii de numere ce trebuie procesate sa nu creasca niciodata peste 10. (Aici va trebui sa folositi direct `java.util.concurrent.ThreadPoolExecutor`).

4. Folosind `Future`-ul general de `ExecutorService::submit` faceti in asa fel incat raspunsurile pentru fiecare numar sunt afisate in aceeasi ordine ca cea a primii numerelor.
