---
title: Implementarea Concurenței în limbaje de programare
subtitle: JAVA 3
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Synchronization using `class Lock`{.java}

## `Lock` vs `synchronized`{.java}

### `synchronized (ob) { ... }`{.java}

- Uses the intrinsic lock of the object
- Forces a structured approach to synchronization:
  -  intrinsic lock automatically released upon exiting the block

### Objects of classes implementing the `Lock` interface

- Manage their own locks

    - more flexible
    - threads can access locks in any order; 
    - making sure the lock is released falls on the programmer
        
        ```java
        Lock l = ...
        l.lock();
        try {
          ...
        } finally { l.unlock(); }
        ```

## Avoiding interference using `Lock`
```java
public class NonInterference {
  static int c = 0;
  public static void main(String[] args)
      throws InterruptedException {
    Lock cLock = new ReentrantLock();
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x)
        {cLock.lock(); try {c++;} finally {cLock.unlock();}}
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x)
      {cLock.lock(); try {c--;} finally {cLock.unlock();}}
    myThread.join();
    System.out.println("c = " + c);
  }
}
```

## `interface Lock`{.java}

`void lock()`{.java}

: Acquires lock associated to object; may block

`void unlock()`{.java}

: Releases lock associated to object; not blocking

`void lockInterruptibly()`{.java}

: Acquires associated lock
  - if the thread is not interrupted; may block

`boolean tryLock()`{.java}

: Acquires the lock if available when invoked; not blocking

`boolean tryLock(long time, TimeUnit unit)`{.java}

: Acquires the lock if it becomes available within the given time

`Condition newCondition()`{.java}

: Creates a new `Condition` variable corresponding to the current `Lock` instance.

## `interface Condition`{.java}

- Implements methods similar to `wait` and `notify` for `Lock`s

    `await(), await(long time, TimeUnit unit)`{.java}

    : Current threads waits for a signal on _this condition_

    `signal()`, `signalAll()`

    : one/all thread(s) waiting for _this condition_ is/are signaled to wake.

- Similarly to `Object`, conditions are linked to a `Lock` object
    - lock must be acquired before calling `wait()`

- Multiple conditions can coexist for the same lock


## Better Producer-Consumer Solution using `Lock` and `Condition` (I)

```java
public class Cell<T> implements DropBox<T> {
  private T cell = null;
  private boolean open = true;
  private Lock cellLock;
  private Condition putCond;
  private Condition getCond;

  public Cell() {
    cellLock = new ReentrantLock();
    putCond = cellLock.newCondition();
    getCond = cellLock.newCondition();
  }
```


## Better Producer-Consumer Solution using `Lock` and `Condition` (`put`)

```java
  @Override public boolean put(T message)
      throws InterruptedException {
    cellLock.lock();
    try {
      while (open && cell != null) putCond.await();
      if (!open) return false;
      cell = message;
      getCond.signal();
      return true;
    } finally {
      cellLock.unlock();
    }
  }
```

## Better Producer-Consumer Solution using `Lock` and `Condition` (`take`)
 
```java
  @Override public Optional<T> take()
      throws InterruptedException {
    cellLock.lock();
    try {
      while (open && cell == null) getCond.await();
      if (open) putCond.signal();
      T message = cell;
      cell = null;
      return Optional.ofNullable(message);
    } finally {
      cellLock.unlock();
    }
  }
```

## Better Producer-Consumer Solution using `Lock` and `Condition` (`close`)

```java
  @Override public void close() {
    cellLock.lock();
    try {
      open = false;
      putCond.signalAll();
      getCond.signalAll();
    } finally {
      cellLock.unlock();
    }
  }
}
```

### Notes

- using different conditions for `put` and `take`
- Advantage: no need to wake all threads each time something changes

# Readers-Writers Model

## Readers-Writers Interaction Model

- Multiple threads need concurrent access to the same resource
- Some threads write (writers), others only read (readers).
- The resource can be read simultaneously by multiple readers
- The resource cannot be written simultaneously
- The resource cannot be simultaneously read and written

### Expressed in Java through the `ReadWriteLock` interface

`readLock()`

: yields the lock for readers

`writeLock()`

: yields the lock for writers

## Addressing non-interference using `ReadWriteLock` (reader thread)

```java
public class NonInterferenceRW {
  static int c;
  public static void main(String[] args)
      throws InterruptedException {
    final ReadWriteLock lock = new ReentrantReadWriteLock();

    Runnable reader = () -> {
      lock.readLock().lock();
      try {
        System.out.println(Thread.currentThread().getName()
            + " counter: " + c);
      } finally { lock.readLock().unlock(); }
    };
```

## Addressing non-interference using `ReadWriteLock` (writer threads)

```java
    Thread incThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) {
        lock.writeLock().lock(); try {
            for (int i = 0; i < 5; i++)
              { c++; Thread.yield(); }
        } finally { lock.writeLock().unlock(); }
      }});

    Thread decThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) {
        lock.writeLock().lock(); try {
            for (int i = 0; i < 5; i++)
              { c--; Thread.yield(); }
        } finally { lock.writeLock().unlock(); }
      }});
```

## Addressing non-interference using `ReadWriteLock` (main setup)

```java
    incThread.start();
    decThread.start();
    ExecutorService executorService =
        Executors.newFixedThreadPool(2);
    for (int i = 0; i < 100; i++)
        executorService.execute(reader);
    executorService.shutdown();
    while (!executorService.isTerminated()) {
      executorService.awaitTermination(1, TimeUnit.SECONDS);
    }
    incThread.join();
    decThread.join();
    System.out.println("c = " + c);
  }
}
```

<!---
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
-->

# `Executor`, `ExecutorService`, `Executors`

## `Executor` and `ExecutorService` interfaces 

### `Executor` is an abstraction over `Thread`

`void execute(Runnable task)`{.java}

: Executes a `task`; whether blocking or non-blocking, implementation decides

### `ExecutorService` enhances the `Executor` functionalities

* ... with ways to control execution

```java
for (int i = 0; i < 100; i++) executorService.execute(reader);
executorService.shutdown();
while (!executorService.isTerminated()) {
  executorService.awaitTermination(1, TimeUnit.SECONDS);
}
```

* ... with ways to wait for results (next section)

## `class Executors`{.java}

Provides static methods for creating `ExecutorService`s

`static ExecutorService newCachedThreadPool()`{.java}

: creates a pool of threads to execute tasks.
  Reuses ended threads. Uses a queue for tasks.

`static ExecutorService newFixedThreadPool(int nThreads)`{.java}

: creates pool of `nThreads` threads to execute tasks.
  Uses a (blocking) queue for scheduling tasks.

`static ExecutorService newSingleThreadExecutor()`{.java}

: creates a single-threaded pool for executing tasks.
  Uses a queue for tasks.


## Asynchronous functions in Java: `Callable`, `Future`

### `Callable<T>`{.java}

* Abstraction for actions (concurrent or not) returning a result
* think of it as a `Runnable` returning a value.

```java
Callable<Integer> giveMeFive = () -> 5;
```

### `Future<T>`{.java}

The promise of an `Executor` to deliver the result of a `Callable`

`T get()`{.java}

: Obtains the result promised by the Future. May block waiting for the action to conclude

### The `submit` method from `ExecutorService`

`Future<T> submit(Callable<T> task)`{.java}

: Schedules a `task` for execution, returning a promise to deliver its result

<!---
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
-->

## `Future`

* `ExecutorService.submit(task)`{.java} returns a `Future` 
  - does not block; calling thread continues executing tasks
  - even for a single-threaded Executor

* Result promised `Future` is obtained using `get()`
  - `get()` blocks calling thread until result becomes available.
  - if the thread executing the `Callable` is interrupted, throws

* `isDone()` called for a `Future` tells whether result is available.
    

## `Future` example using `isDone`

```java
public class FutureExample {
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

## `ExecutorService`: `invokeAll` and `invokeAny`

`List<Future<T>>`
 
`  invokeAll(Collection<? extends Callable<T>> tasks)`

: Schedules all tasks for execution

    - returns a `Future` for each task
    - Blocks untill all tasks are done.

`T invokeAny(Collection<? extends Callable<T>> tasks)`

: Schedules all taks for execution

    - returns one of the results
    - the other tasks are canceled

## `invokeAll` example

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

## `invokeAny` example

* Allows concurrent execution of multiple tasks
* Choses the task finishing first 
* Useful when having more methods for solving the same problem

```java
int search(Integer key)
    throws ExecutionException, InterruptedException {
  return executor.invokeAny(Arrays.asList(
          () -> search1(key),
          () -> search2(key),
          () -> search3(key)));
}
```

# Sequential consistency

## Concurrent objects and serial specification

### Concurrent objects

Shared entities upon which a thread can perform certain operations

### Serial specification

The valid behavior of a concurrent object (in isolation)
    
* we only observe the operations performed on that object

## Concurrent object example: shared memory location

### Operations

read

: Reading the value from the memory location

write

: Writing a value into the memory location

### Serial specification

Every `read` operation yields the value written by the latest `write`.


## Concurent object example: mutex (lock)

### Operations

lock

: acquires the lock

unlock

: releases the lock

### Serial specification

* `#lock - #unlock` is either `0` or `1` for all prefixes
    * sequence of `lock unlock` pairs
    * last `unlock` can miss
* Consecutive `lock` - `unlock` operations belong to same thread

<!---
## Exemplu de obiect concurent: semafor cu `n` resurse inițiale

### Operații

acquire

: achiziționarea unei resurse

release

: eliberarea unei resurse

### Specificația serială

In orice moment `n + #release - #acquire >= 0`
  * nu pot obține mai multe resurse decât disponibile

-->


## Formalizing sequential consistency [Attiya&Welch, 1994]

### Legal execution

* Any concurrent object satisfies its serial specification
* When restricting the execution to that object alone.

### Execution reordering

A permutation of the events in the original execution such that
its restriction to each thread is the same as in the original execution

### Sequential consistent execution

An execution admitting a _legal reordering_

### Guarantees offered by sequential consistency

* Execution happens _as if_ it were legal

* Programmer can assume each concurrent object behaves according to its serial specification

## The Java memory model

* Sequential consistency <= program is _correctly synchronized_

Correctly synchronized

:  no _data-races_

Data-race 

: _conflictual access_ not ordered by _happens-before_
: concurrent access to same location, one is `write`

### The Happens-before relation  `hb(x,y)`

_Thread order_

: x before y within the same thread => hb(x,y)

_Synchronization_

: x _synchronizes with_ subsequent action y => hb(x,y)

_Transitivity_

: hb(x,y) and hb(y,z) => hb(x,z)

_Constructor-Finalizer_

: If x is the end of a constructor and y is the call to `finalize` for that object => hb(x,y)

## Synchronization order

* `unlock` _synchronizes with_ subsequent `lock`s on same monitor
* `write` of `volatile`{.java} `v` _synchronizes with_ subsequent `read`s of `v`
* Thread start _synchronizes with_ first operation in that thread
* Final operation in thread _synchronizes with_ any operation detecting that the thread has ended
