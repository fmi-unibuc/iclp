---
title: Implementarea Concurenței în limbaje de programare
subtitle: JAVA 3
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
thanks: Bazat pe cursul ținut de dna. prof. Ioana Leuștean
abstract: |
---

# Modalități de sincronizare de nivel scăzut

## Variabile atomice

* Implementate folosind instrucțiuni hardware compare-and-swap
* Atomicitate fără sincronizare. mult mai rapide decât cu locks
* `AtomicBoolean`, `AtomicInteger`, ...

### Metode pentru `AtomicInteger`

`int incrementAndGet()`

: crește valoarea cu o unitate și întoarce valoarea nouă

`int addAndGet(int delta)`

: adaugă `delta` (poate fi negativă) și întoarce valoarea nouă

`boolean compareAndSet(int old, int new)`

: dacă valoarea curentă e `old` setează valoarea la `new`

## Exemplu: Eliminarea interferenței folosind AtomicInteger

```java
public class NonInterferenceAtomic {
  static AtomicInteger c = new AtomicInteger(0);
  public static void main(String[] args)
      throws InterruptedException {
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) c.incrementAndGet();
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x) c.decrementAndGet();
    myThread.join();
    System.out.println("c = " + c.get());
  }
}
```

## Semafoare

### Definiție

Modelează accesul (concurent) la (mai multe) resurse de același tip

### Metode

`Semaphore(int permits)`{.java}

: constructorul clasei, declară câte resurse sunt disponibile la început

`void acquire()`

: solicită o resursă; dacă nu sunt resurse disponibile, așteaptă

`void release()`

: oferă o resursă

### Observații

* Se poate obține excludere mutuală (Lock) cu un semafor cu o resursă

* Un semafor e mai flexibil decât un Lock deoarece

    - `acquire` și `release` sunt independente una de cealaltă
    - Numărul de resurse este variabil
    
## Exemplu cu semafoare (2 resurse accesibile)

```java
  static class Resource implements Runnable {
    private Semaphore semaphore = new Semaphore(2);

    @Override public void run() {
      try {
        semaphore.acquire(); //request resource
        try { 
          for (int i = 0; i < 3; i++) {
            Thread.sleep(1000); // access resource
          }
        } finally {
          semaphore.release(); // release resource
        }
      } catch (InterruptedException e) { /* ... */ }
    }
  }
```

## Exemplu cu semafoare (main method)
```java
  public static void main(String[] args)
          throws InterruptedException {
    Runnable resource = new Resource();
    Thread[] threads = new Thread[4];
    for (int i = 0; i < 4; i++) {
      threads[i] = new Thread(resource, "Thread " + i);
      threads[i].start();
    }
    for (int i = 0; i < 4; i++) {
      threads[i].join();
    }
  }
```

## Exemplu de rulare

```
Thread 0 requests resource.
Thread 1 requests resource.
Thread 3 requests resource.
Thread 2 requests resource.
Thread 0 acquired resource.
Thread 3 acquired resource.
Thread 3: performs operation 0
Thread 0: performs operation 0
Thread 0: performs operation 1
Thread 3: performs operation 1
Thread 0: performs operation 2
Thread 3: performs operation 2
Thread 0 releases resource.
Thread 3 releases resource.
Thread 2 acquired resource.
Thread 1 acquired resource.
Thread 2: performs operation 0
Thread 1: performs operation 0
Thread 2: performs operation 1
Thread 1: performs operation 1
Thread 2: performs operation 2
Thread 1: performs operation 2
Thread 2 releases resource.
Thread 1 releases resource.
```

# `Executor`, `ExecutorService`, `Executors`

## Interfețele `Executor` și `ExecutorService` 

### `Executor` abstractizează ideea de `Thread`

`void execute(Runnable task)`{.java}

: Execută un `task`; nu garantează cum (blocant / ne-blocant)

### `ExecutorService` extinde funcționalitățile de `Executor`

* Cu modalități de a controla execuția

```java
for (int i = 0; i < 100; i++) executorService.execute(reader);
executorService.shutdown();
while (!executorService.isTerminated()) {
  executorService.awaitTermination(1, TimeUnit.SECONDS);
}
```

* Cu modalități de a aștepta rezultatele (secțiunea următoare)

## Clasa `Executors`

* Oferă metode statice pentru a crea servicii Executor

`static ExecutorService newCachedThreadPool()`{.java}

: crează un grup de thread-uri pentru a executa task-uri.
    Refolosește thread-urile terminate. Folosește o coadă pentru task-uri

`static ExecutorService newFixedThreadPool(int nThreads)`{.java}

: crează un grup de `nThreads` thread-uri pentru a executa task-uri.
    Folosește o coadă pentru task-uri

`static ExecutorService newSingleThreadExecutor()`{.java}

: folosește un singur `Thread` pentru a executa task-uri.
    Folosește o coadă pentru task-uri.
...


## Funcții asincrone: `Callable`, `Future`

### `Callable<T>`{.java}

* modelează funcții / acțiuni (concurente) care întorc un rezultat de tip `T`
* gândit ca un `Runnable` care întoarce ceva

```java
Callable<Integer> giveMeFive = () -> 5;
```

### `Future<T>`{.java}

Reprezintă promisiunea unui executor de a livra rezultatul unui `Callable`

`T get()`{.java}

: Produce rezultatul promis. Potențial așteaptă ca acțiunea asociată să se încheie.

### Metoda `submit` din `ExecutorService`

`Future<T> submit(Callable<T> task)`{.java}

: Planifică `task` pentru execuție și întoarce o promisiune de a livra rezultatul

## Exemplu cu `Future`: evaluare paralelă asincronă

```java
static class AsyncBinaryOperation<T> implements Callable<T> {
  private final BinaryOperator<T> op;
  private final Future<T> left;
  private final Future<T> right;

  public AsyncBinaryOperation
      ( Callable<T> left, Callable<T> right
      , BinaryOperator<T> op) {
    this.op = op;
    this.left = executor.submit(left);
    this.right = executor.submit(right);
  }

  @Override public T call() throws Exception
    { return op.apply(left.get(), right.get()); }
}
```

## `Future`

* `ExecutorService.submit(task)`{.java} revine din execuție imediat,
    returnând un obiect `Future`.
    Din acest moment se pot executa diferite task-uri în parallel cu cea
    al cărui rezultat va fi comunicat obiectului `Future`. 

* Rezultatul comunicat obiectului `Future` este obținut apelând metoda `get()`.

* Metoda `get()` a obiectelor `Future` va bloca thread-ul care o apelează până
    când se comunică rezultatul către obiectul `Future`; dacă task-ul executat
    de obiect este anulat, metoda get() aruncă excepție.

* Metoda `isDone()` a obiectelor `Future` poate fi apelată pentru a vedea 
    dacă task-ul asociat s-a finalizat și rezultatul este disponibil.
    

## `Future` exemplu `isDone`

```java
public class FutureExample2 {
 public static void main(String[] args) throws Exception {
  ExecutorService executor =
    Executors.newSingleThreadExecutor();
  Future<Integer> future = executor.submit(() -> {
   int time = ThreadLocalRandom.current().nextInt(1000, 5000);
   Thread.sleep(time); return time;
  });
  while (!future.isDone())
   { System.out.println("Task not done ...");
     Thread.sleep(500); }
  System.out.println("Task duration: " + future.get());
  executor.shutdown();
 }
}
```

## `ExecutorService`: `invokeAll` și `invokeAny`

`List<Future<T>>`
 
`  invokeAll(Collection<? extends Callable<T>> tasks)`

: Execută task-urile (în paralel) și întoarce o listă de promisiuni pentru 
    rezultatele lor
    
    - Se blochează până când toate task-urile se termină

`T invokeAny(Collection<? extends Callable<T>> tasks)`

: Execută task-urile (în paralel) și întoarce unul dintre rezultate.

    - Task-urile neterminate sunt anulate 

## Exemplu `invokeAll`

```java
Optional<To> mapReduce(Collection<From> c,
                     Function<From, To> map,
                     BinaryOperator<To> reduce)
                     throws Exception {
  return
    executor.invokeAll(
      c.stream()
        .map( e -> ((Callable<To>) () -> map.apply(e)) )
        .collect(Collectors.toList())
    ).stream()
      .map(InvokeAllExample::get)
      .reduce(reduce);
}
```



## Exemplu `invokeAny`

* Permite excuția în paralel a mai multe task-uri care rezolvă aceeași problemă
* și alegerea celui care termină mai rapid.

```java
int search(Integer key)
    throws ExecutionException, InterruptedException {
  return executor.invokeAny(Arrays.asList(
          () -> search1(key),
          () -> search2(key),
          () -> search3(key)));
}
```