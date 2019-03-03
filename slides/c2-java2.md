---
title: Implementarea Concurenței în limbaje de programare
subtitle: JAVA 2
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
thanks: Bazat pe cursul ținut de dna. prof. Ioana Leuștean
abstract: |
---

# Modelul Producător-Consumator

## Modelul Producător-Consumator 

```
              --------
 ----------> | BUFFER | --------->
 producător   --------  consumator
```

### Descrierea modelului

Două threaduri comunică prin intermediul unui buffer (memorie partajată):

- thread-ul __Producator__ crează datele și le pune în buffer
- thread-ul __Consumator__ ia datele din buffer și le prelucrează

### Probleme de coordonare

- Producătorul și consumatorul nu vor accesa bufferul simultan.
- Producătorul nu va pune în buffer date noi dacă buffer-ul e plin
- Thread-urile se anunță reciproc când starea buferului se schimbă


## Interfața pentru buffer

```java
public interface DropBox<T> {
  /**
   * Puts a message in the dropbox
   * @return whether the operation succeeded (dropbox open)
   */
  boolean put(T message) throws InterruptedException;
  /**
   * Takes a message from the dropbox
   * @return an optional message, if dropbox open
   */
  Optional<T> take() throws InterruptedException;
  /**
   * Closes the dropbox, making calls invalid
   */
  void close();
}
```

## Implementare producător

```java
public class ProducerThread<T> implements Runnable {
  private final DropBox<T> box;
  private Collection<T> items;

  public ProducerThread(DropBox box, Collection<T> items)
    { this.box = box; this.items = items; }

  @Override public void run() {
    Random rnd = new Random();
    try {
      for (T item : items)
        { box.put(item); Thread.sleep(rnd.nextInt(2000)); }
    } catch (InterruptedException e)
      System.err.println("Got interrupted!");
  }
}
```

## Implementare consumator

```java
public class ConsumerThread<T> implements Runnable {
  private final DropBox<T> box;
  private final Consumer<T> handle;

  public ConsumerThread(DropBox<T> box, Consumer<T> handle)
    { this.box = box; this.handle = handle; }

  @Override public void run() {
    try {
      Optional<T> message = box.take();
      while (message.isPresent())
        {handle.accept(message.get()); message = box.take();}
    } catch (InterruptedException e)
      System.err.println("Got interrupted!");
  }
}
```


## Implementare buffer cu un singur element I (`close` și `put`)

```java
public class Cell<T> implements DropBox<T> {
  private T cell = null;
  private boolean open = true;

  @Override public synchronized void close()
    { open = false; notifyAll(); }

  @Override public synchronized boolean put(T message)
      throws InterruptedException {
    while (open && cell != null) wait();
    if (!open) return false;
    cell = message;
    notifyAll();
    return true;
  }
```

## Implementare buffer cu un singur element II (`take`)
 
```java
  @Override public synchronized Optional<T> take()
      throws InterruptedException {
    while (open && cell == null) wait();
    if (open) notifyAll();
    T message = cell;
    cell = null;
    return Optional.ofNullable(message);
  }
}
```

### Observații

- implementarea folosește blocuri `synchronized`{.java} cu gărzi `wait`{.java}
- thread-ul este suspendat până când o anume condiție este satisfăcută


## Exemplu de `main`

```java
  public static void main(String [] args)
      throws InterruptedException {
    DropBox<String> box = new Cell<>();
    ProducerThread<String> p1 = new ProducerThread<>(box,
        Arrays.asList("This", "is", "important"));
    ProducerThread<String> p2 = new ProducerThread<>(box,
        Arrays.asList("so", "incredibly", "much", "highly"));
    ConsumerThread<String> c = new ConsumerThread<>(box,
        message -> System.out.format("%s received %s%n",
                Thread.currentThread().getName(), message));
    Thread pt1 = new Thread(p1); Thread pt2 = new Thread(p2);
    Thread ct1 = new Thread(c); Thread ct2 = new Thread(c);
    pt1.start(); pt2.start(); ct1.start(); ct2.start();
    pt1.join(); pt2.join(); box.close();
    ct1.join();ct2.join(); System.out.println("DONE");
  }
```

## Exemplu de rulare

```
Thread-2 received This
Thread-3 received so
Thread-2 received is
Thread-3 received incredibly
Thread-2 received important
Thread-3 received much
Thread-2 received highly
DONE
```

# Sincronizare folosind `Lock`

## `Lock` vs `synchronized`{.java}

### `synchronized (ob) { ... }`{.java}

- Accesează lacătul intern al resursei 
- impune o programare structurată:
    resursa e eliberată automat la ieșirea din bloc

### Obiectele claselor care implementează interfața `Lock`

- Nu accesează lacătul resursei ci propriul  lor lacăt,

    - mai multă flexibilitate
    - thread-urile pot accesa lacătele în orice ordine; 
    - este responsabilitatea programatorului să se asigure că lock-ul e eliberat
        
        ```java
        Lock l = ...
        l.lock();
        try {
          ...
        } finally { l.unlock(); }
        ```

## Eliminarea interferenței folosind `Lock`
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

## Metodele interfeței `Lock`

`void lock()`{.java}

: Obține lacătul asociat obiectului. Se poate bloca.

`void unlock()`{.java}

: Eliberează lacătul asociat obiectului. Nu se blochează.

`void lockInterruptibly()`{.java}

: Obține lacătul asociat dacă thread-ul nu e întrerupt. Se poate bloca.

`boolean tryLock()`{.java}

: Obține lacătul dacă este disponibil la momentul invocării. Nu se blochează.

`boolean tryLock(long time, TimeUnit unit)`{.java}

: Obține lacătul dacă devine disponibil în intervalul dat și dacă thread-ul nu e întrerupt.

`Condition newCondition()`{.java}

: crează o nouă instanță de `Condition` corespunzătoare instanței curente `Lock`.

## Interfața `Condition`{.java}

- Implementează metode asemănătoare cu `wait`, `notify` pentru obiectele din clasa `Lock`

    `await(), await(long time, TimeUnit unit)`{.java}

    : thread-ul curent intră în așteptare

    `signal()`, `signalAll()`

    : un thread/toate thread-urile care așteaptă după condiția curentă e trezit

- Conditiile sunt legate de un obiect `Lock`
    - lock-ul trebuie obținut înainte de a apela `wait()`

- Pot exista mai multe condiții pentru același obiect `Lock`.


## Implementare `Cell` folosind `Lock` și `Condition` (I)

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


## Implementare `Cell` folosind `Lock` și `Condition` (`put`)

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

## Implementare `Cell` folosind `Lock` și `Condition` (`take`)
 
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

## Implementare `Cell` folosind `Lock` și `Condition` (`close`)

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

### Observații

- implementarea folosește condiții diferite pentru `put` și `take`
- Avantaj: nu mai este necesar să trezim toate thread-urile mereu

# Modelul Cititori–Scriitori

## Modelul de interacțiune Cititori-Scriitori

- Mai  multe thread-uri au acces la  o resursă.
- Unele thread-uri scriu (writers), iar altele citesc (readers).
- Resursa poate fi accesată simultan de mai mulți cititori.
- Resursa poate fi acessată de un singur scriitor.
- Resursa nu poate fi accesată simultan  de cititori și de scriitori

### Exprimată în Java prin interfața `ReadWriteLock`

`readLock()`

: întoarce lacătul pentru cititori

`writeLock()`

: întoarce lacătul pentru scriitori

## Problema non-interferenței folosind `ReadWriteLock` (reader thread)

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

## Problema non-interferenței folosind `ReadWriteLock` (writer threads)

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

## Problema non-interferenței folosind `ReadWriteLock` (main setup)

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
