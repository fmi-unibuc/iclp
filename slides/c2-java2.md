---
title: Implementarea Concurenței în limbaje de programare
subtitle: JAVA 2
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# The Producer-Consumer Problem

## The Producer-Consumer Problem

```
              --------
 ----------> | BUFFER | --------->
 producer     --------  consumer
```

### Problem description

Two threads communicate through a buffer (shared memory):

- The __Producer__ "produces" data and puts it in a buffer
- The __Consumer__ takes the data from the buffer and "consumes" it

### Coordination problems

- Producer and consumer should not access the buffer simultaneously
- Producer must wait if the buffer is full
- Consumer must wait if the buffer is empty
- Producer and Consumer should let each-other know when buffer state changes

## Buffer interface

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
   * Closes the dropbox, making subsequent calls invalid
   */
  void close();
}
```

## Implementing the producer

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

## Implementing the consumer

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


## Synchonization control through the `Object`{.java} class

`void wait()`{.java}, `void wait(long milisecunde)`{.java}

: the thread enters the WAITING state, waiting to receive a  `notifyAll`{.java} or a `notify`{.java} signal for the object's intrinsic lock

`void notifyAll()`{.java}

: wakes up all threads waiting on this object's intrinsic lock

`void notify()`{.java}

: wakes up a single thread waiting on this object's intrinsic lock

    - the thread is randomly chosen

## `object.wait()`

- Must be called from within a block synchronized on `object`
- Frees `object`'s intrinsic lock
- Waits until notified by `notify`{.java}/`notifyAll`{.java}
- When notified, it attempts to reaquire `object`'s intrinsic lock
- Throws an `InterruptedException` if the thread is interrrupded while in `WAITING` state

```java
synchronized (obj) {
  while (<condition does not hold>)
    obj.wait();
  ... // Perform action appropriate to condition
}
```

### Important

_Always_ enclose `obj.wait()` in a loop due to  _spurious wakeup_


## Implementing a buffer of size 1 (`close` și `put`)

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

## Implementing a buffer of size 1 (`take`)
 
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

### Note

- Using `synchronized`{.java} methods to prevent simultaneous access
- Using `wait`{.java} guards to block when buffer is full/empty
  - Thread is suspended until receiving `notify`{.java} by partener thread


## A possible `main` function for Producer-Consumer

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

## Possible execution trace

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

## BlockingQueue<E>{.java}

void put(E e) throws InterruptedException{.java}

: adds element to the queue; waits if queue is full (blocking)

boolean offer(E e){.java}

: adds element to the queue if possible; returns true upon success (non-blocking)

E take() throws InterruptedException{.java}

: retrieves an element from the queue; waits if queue is empty (blocking)

E poll(){.java}

: retrieves an element from the queue, if available; returns true upon success (non-blocking)

## Classes implementing BlockingQueue<E>{.java}

ArrayBlockingQueue

: Bounded queue (the standard producer-consumer buffer)

DelayQueue

: Queue of events with delays (expiration times).
  Element can only be taken when its delay has expired.

LinkedTransferQueue

: Unbounded queue; producer may choose to wait for element to be consumed

SynchronousQueue

: a zero-sized queue (synchronous communication channel)

PriorityBlockingQueue

: Unbounded priority queue (heap)
