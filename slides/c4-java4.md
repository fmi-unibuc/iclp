---
title: Implementarea Concurenței în limbaje de programare
subtitle: JAVA 4
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
thanks: Bazat pe cursul ținut de dna. prof. Ioana Leuștean
abstract: |
---


# Programare funcțională concurentă folosind `Future`s

## Serviciul de execuție `ForkJoinPool`

* Folosește metoda work-stealing pentru a minimiza sincronizarea
* Ideal pentru task-uri care presupun execuția altor task-uri mai mici

* număr de thread-uri fixate
    * implicit, numărul disponibil de thread-uri ale sistemului
* fiecare thread menține o coadă de task-uri
* thread-urile libere iau task-uri dîn cozile thread-urilor ocupate

## `ForkJoinPool` și `ForkJoinTask`

`ForkJoinPool` planifică pentru execuție obiecte `ForkJoinTask`

`public static ForkJoinPool commonPool()`{.java}

: oferă o instanță a clasei; asemănătoare metodelor din clasa `Executors`

`public void execute(ForkJoinTask<?> task)`{.java}

: planifică `task` pentru execuție (asincronă)

`public <V> ForkJoinTask<V> submit(ForkJoinTask<V> task)`{.java}

: planifică `task` pentru execuție și întoarce un `Future` (task)

`public <V> V invoke(ForkJoinTask<T> task)`{.java}

: Execută `task`, întorcând rezultatul la final

## `ForkJoinTask` și `RecursiveTask`

### `abstract class ForkJoinTask<V> implements Future<V>`{.java}

`public final ForkJoinTask<V> fork()`{.java}

: Cheamă `submit(this)` pentru obiectul `ForkJoinPool` atașat

`public final V join()`{.java}

: Întoarce rezultatul task-ului atunci când acesta e disponibil


### `abstract class RecursiveTask<V> extends ForkJoinTask<V>`{.java}

`protected abstract V compute()`{.java}

: funcția implementată de acest task

* Un `ForkJoinTask` menit pentru a descrie funcții recursive
* `compute()` poate crea și executa (asincron) sub-funcții

## Evaluarea expresiilor folosind `RecursiveTask` (`Number)

```java
abstract class Expression extends RecursiveTask<BigInteger> {
  static class Number extends Expression {
    BigInteger value;
    Number(long x) { value = BigInteger.valueOf(x); }

    @Override
    public BigInteger compute() { return value; }
  }
```

* Expresiile extind `RecursiveTask`
* Funcția `compute()` calculează valoarea expresiei
* pentru `Number`, rezultatul e direct valoarea

## Evaluarea expresiilor folosind `RecursiveTask` (`Binary`)
```java
  public static class Binary extends Expression {
    private final BinaryOperator<BigInteger> op;
    private final Expression left;
    private final Expression right;

    Binary(BinaryOperator<BigInteger> op,
           Expression left, Expression right)
    { this.op = op; this.left = left; this.right = right; }

    @Override protected BigInteger compute() {
      right.fork(); // start evaluating right as task
      return op.apply(left.compute(), right.join());
      // compute left here, wait for right, apply op
    }
  }
```

## Evaluarea expresiilor folosind `RecursiveTask` (`main`)
```java
  public static void main(String[] args) {
    Expression e = parse(args[0]);
    System.out.println("Value: " + e.invoke());
  }
}
```

* Funcția `parse` e la fel ca în cursul trecut
* Calculăm valoarea lui `e` aici folosind `invoke`

# Consistență secvențială

## Obiecte concurente și specificație serială

### Obiecte concurente

Entități care exportă anumite operații ce pot fi efectuate asupra lor de un thread.

### Specificație serială

Comportamentul valid al unui obiect concurent în izolare
    
    * dacă ne uităm doar la operațiile efectuate asupra lui

## Exemplu de obiect concurent: o locație de memorie

### Operații

read

: Citirea valorii de la locația de memorie

write

: Scrierea unei valori la acea locație

### Specificația serială

Fiecare operație de citire întoarce valoarea scrisă de cea mai recentă operație de scriere.


## Exemplu de obiect concurent: mutex (lock)

### Operații

lock

: obținerea lacătului

unlock

: eliberarea lacătului

### Specificația serială

* `#lock - #unlock` este fie `0` fie `1` 
    * secvență de perechi `lock unlock`
    * ultimul `unlock` poate lipsi
* operațiile consecutive lock - unlock efectuate de același thread


## Exemplu de obiect concurent: semafor cu `n` resurse inițiale

### Operații

acquire

: achiziționarea unei resurse

release

: eliberarea unei resurse

### Specificația serială

In orice moment `n + #release - #acquire >= 0`
  * nu pot obține mai multe resurse decât disponibile

## Definirea consistenței secvențiale [Attiya&Welch, 1994]

### Execuție legală

Restricția execuției la orice obiect concurent satisface specificația sa serială

### Reordonarea unei execuții

Restricția execuției reordonate la orice fir de execuție e aceeași ca în execuția originală

### Execuție secvențial consistentă

O execuție care admite o *reordonare legală*

### Garanții oferite de consistența secvențială

* Execuția are loc *ca și cum* ar fi legală.

* Programatorul poate presupune că fiecare obiect concurent se comportă comform specificației sale seriale.

## Modelul de memorie al limbajului Java

* Program *corect sincronizat* => execuții secvențial consistente
* Program corect sincronizat <= fără *data-races*
* Data-race <= *accesări conflictuale* neordonate de relația *happens-before*
* Două accesări ale aceleiași locatii de memorie sunt conflictuale dacă una din ele este de scriere.

### Relația Happens-before  hb(x,y)

**Ordinea în thread**

: x înaintea lui y în același thread => hb(x,y)

**Sincronizare**

: x *se sincronizează* cu acțiunea ulterioară y => hb(x,y)

**Tranzitivitate**

: hb(x,y) și hb(y,z) => hb(x,z)

**Constructor-Finalizer**

: Dacă x e sfărșitul execuției unui constructor și y e apelul metodei `finalize` pentru acel obiect => hb(x,y)

## Ordinea de sincronizare

* Operația unlock asupra unui monitor `m` se sincronizeaza cu orice lock ulterior asupra lui `m`
* Operația write asupra unei variabile `volatile`{.java} `v` se sincronizează cu operatiile ulterioare read aasupra lui v
* Operația de pornire a unui thread se sincronizează cu prima operatie din acel thread.
* Ultima operație dintr-un thread se sincronizează cu orice operație care detectează ca thread-ul s-a terminat

# Colecții concurente

## Colecții concurente

### Rol

Evită erorile de consistență a memoriei asigurând o relație de tip happens-before între o operație care adaugă un element și operațiile ulterioare care accesează/modifică/elimină acel obiect.

### Interfețe (`java.util.concurrent`)

`BlockingQueue` 

: definește o coadă care se blochează la adăugare când e plină și la scoatere când e goală

`ConcurrentMap`

: definește un `Map` care e sigur pentru operații din mai multe thread-uri și care oferă garanții de atomicitate

`

## `BlockingQueue<E>`{.java}

`void put(E e) throws InterruptedException`{.java}

: adaugă elementul în coadă; așteaptă dacă nu mai e loc

`boolean offer(E e)`{.java}

: adaugă elementul în coadă dacă e loc; întoarce dacă a reușit

`E take() throws InterruptedException`{.java}

: ia un element din coadă; așteaptă dacă nu sunt elemente disponibile

`E poll()`{.java}

: ia un element din coadă, dacă e disponibil; întoarce dacă a reușit

### Implementări

`ArrayBlockingQueue`

: o coadă marginită (producător - consumator)

`DelayQueue`

: O coadă de elemente cu `Delay` (evenimente cu expirare)

`LinkedTransferQueue`

: producătorul poate alege să aștepte consumarea

`SynchronousQueue`

: o coadă de lungime 0 (canal sincron de comunicare)

`PriorityBlockingQueue`

: o coadă cu priorități (heap) nemărginită

## `ConcurrentHashMap<K,V>`{.java}

* Tabelă de dispersie concurentă
    * Aceeași interfață ca `Hashtable`

* Sigur pentru operații din mai multe thread-uri
    * fără însă a folosi lock-uri pentru sincronizare
    * concurență maximă la regăsire
    * concurență crescută la actualizare

* Operații asupra întregii colecții în paralel cu operații de actualizare
    * `forEach`, `search`, `reduce`
    * folosesc ForkJoinPool pentru a paraleliza task-urile.
    * optional - prag număr minim de elemente pentru a folosi mai multe thread-uri

