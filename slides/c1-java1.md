---
title: Implementarea Concurenței în limbaje de programare
subtitle: Threads and shared memory --- JAVA
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Threads and shared memory [^osBook]

[^osBook]: [Operating Systems Concepts, 9th edition, by A. Silberschatz, P. B. Galvin, and G. Gagne](https://codex.cs.yale.edu/avi/os-book/OS9/index.html)


## Threads

* Most modern applications are multithreaded
* Multiple tasks with the application can be implemented by separate threads
  * Update display
  * Fetch data
  * Spell checking
  * Answer a network request
* Can simplify code, increase responsiveness

## Example (Service Server Arhitecture)

![](images/server.png)

## Benefits

- *Responsiveness*
  + may allow continued execution if part of process is blocked
  + especially important for user interfaces
- *Resource Sharing*
- *Scalability*
  + process can take advantage of multiprocessor architectures

## Concurrency vs parallelism

### Concurrent execution on single-core system:

![](images/single-core.png)

### Execuție concurentă în paralel pe un sistem multi-core:

![](images/multi-core.png)

## Single vs Multithreaded process

![](images/multi-threaded.png)

## Shared Memory

### Benefits

- Efficiency: all threads have direct acecss to the memory
- Simplicity: Accessing shared memory using same model as for sequential processes

### Problems (the need for synchronization)
- Threads may be interruped at any time
  - e.g., while performing a data-sensitive operation
- Concurrent access to shared data may result in data inconsistency
- Maintaining data consistency requires mechanisms to ensure the orderly execution of cooperating threads

## Synchronization -- Mutex (MUTual EXclusion)

- Special synchronization object

- Protects a "critical section" of code
  - (sensitive to concurrent modifications)

- Two zones protected by same mutex cannot interrupt each-other


### Example - mutex m


+-----------------------+-----------------------+ 
|First Thread           | Second Thread         | 
+=======================+=======================+ 
|```                    | ```                   | 
|do stuff               | do stuff              | 
|synchronized(m):       | synchronized(m):      | 
|   output results      |   output results      | 
|do other stuff         | do other stuff        | 
|```                    | ```                   | 
+-----------------------+-----------------------+ 


# Concurrency primitives in Java

## Threads

Any executing thread is an instance of class `Thread`{.java} 

### Thread attributes

- __ID__: unique thread identifier
    - getter: `getId`{.java}, cannot be set
            
- __Name__: thread name (`String`{.java})
    - getter/setter: `getName`{.java}, `setName`{.java}

- __Priority__: thread priority (integer between 1 and 10)
    - getter/setter: `getPriority`{.java}, `setPriority`{.java}
    - Larger number usually means greater execution priority
    - changing priority does not guarantee actual priority 

- __State__: Thread state
    - getter: `getState`{.java}, cannot be set

## [Thread state](https://docs.oracle.com/javase/7/docs/api/java/lang/Thread.State.html)

`public static enum Thread.State extends Enum<Thread.State>`{.java}

- __`NEW`__: thread which has not yet started.
- __`RUNNABLE`__: thread which can execute/is executing
- __`BLOCKED`__:  blocked, waiting for a monitor lock
- __`WAITING`__: waiting for a signal from another thread
- __`TIMED_WAITING`__: waiting thread with a specified waiting time.
- __`TERMINATED`__: thread which has finished executing.

A thread can only be in one of these states at any given time.

## Thread life-cycle  (source: [HowToDoInJava.com](https://howtodoinjava.com/java/multi-threading/java-thread-life-cycle-and-thread-states/))

![Thread states and transitions between them](https://cdn1.howtodoinjava.com/wp-content/uploads/2016/04/Java-Thraed-Life-Cycle-States.jpg)


## `Thread`{.java} creation

### Direct

- by deriving `Thread`{.java} class
- by implementing the `Runable`{.java} interface

### Abstract

- Using the `Executors`{.java} class

## Thread creation using `Runnable`{.java}

### Standard
```java
public class HelloRunnable implements Runnable {
  public void run() { System.out.println("Hello thread!"); }
  public static void main(String args[]) {
     Thread t = new Thread (new HelloRunnable());
     t.start();
  } }
```

### Java 8 (funcții anonime)
```java
public class HelloThread {
  public static void main(String args[]) {
    new Thread( () -> System.out.println("Hello thread!")
              ).start();
  } }
```

## Definirea unui thread ca subclasa a clasei `Thread`{.java}

```java
public class HelloThread extends Thread {

  public void run() {
    System.out.println("Hello thread!");
  }

  public static void main(String args[]) {
    new HelloThread().start();
  }
}
```

## `public class Thread implements Runnable`

- Dynamic methods (called on a `Thread` object)
   
   start()
   
   : A new execution thread is created and the JVM invokes the `run()`{.java} method
             
   join()
   
   : waits for the given thread to finish execution
   
   interrupt()
   
   : asks the given thread to interrupt its execution
   
   boolean isAlive()
   
   : tests whether the thread is alive


- Static methods (applying to the current thread)
   
   yield()
   
   : current thread is willing to yield the processor
   
   sleep(long milis)
   
   : current threads sleeps for given time
   
   Thread currentThread()
                 
   : Returns a reference to the currently executing thread

## JVM threads (source: [docs.oracle.com](https://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html))


When a Java Virtual Machine starts up, there is usually a single non-daemon thread[^daemon], which typically calls the method named main of some designated class.

The Java Virtual Machine continues to execute threads until either:

- The exit method of class Runtime has been called and the security manager permits the exit operation to take place.
- All threads that are not daemon threads have died, either 
  - by returning from the call to the run method or
  - by throwing an exception that propagates beyond the run method.

[^daemon]: Daemon thread: low priority thread servicing user threads (e.g. garbage collector thread)

## `Thread.sleep()` and `InterruptedException`

`sleep()` throws an exception if the thread is interruped while still sleeping.

```java
public class SleepyMessages {
  public static void main(String args[])
      throws InterruptedException {
    String importantInfo[] = 
        { "This", "is", "very", "important"};

    for (int i = 0;  i < importantInfo.length; i++) {
      Thread.sleep(4000);//Pause for 4 seconds
      System.out.println(importantInfo[i]);
    }
  }
}
```

## `sleep`, handling the `InterruptedException`

```java
public class MessageLoop implements Runnable {
  public void run() {
    String importantInfo[] =
        {"This", "is", "very", "important"};
    try {
      for (int i = 0; i < importantInfo.length; i++) {
        Thread.sleep(4000);//Pause for 4 seconds
        threadMessage(importantInfo[i]);
      }
    } catch (InterruptedException e) {
      threadMessage("I wasn't done!");
    }
  }
```

## `currentThread`, `getName`

\small
```java
  public static void threadMessage(String message) {
    String threadName = Thread.currentThread().getName();
    System.out.format("%s: %s%n", threadName, message);
  }

  public static void main(String args[])
      throws InterruptedException {
    threadMessage("Starting MessageLoop thread");
    Thread t = new Thread(new MessageLoop());
    t.start();
    threadMessage("Waiting for MessageLoop thread to finish");
    t.join();
    threadMessage("Finally!");
  }
}
```

## `isAlive`, `join` with timeout, and `interrupt`

\small
```java
public class MessageLoopInterrupted {
  public static void main(String args[])
      throws InterruptedException {
    long patience = 1000 * 10;
    long startTime = System.currentTimeMillis();
    threadMessage("Starting MessageLoop thread");
    Thread t = new Thread(new MessageLoop()); t.start();
    threadMessage("Waiting for MessageLoop thread to finish");
    while (t.isAlive()) {
      threadMessage("Still waiting..."); t.join(2000);
      if ((System.currentTimeMillis() - startTime
           > patience)  && t.isAlive()) {
        MessageLoop.threadMessage("Tired of waiting!");
        t.interrupt(); t.join();
      } }
    MessageLoop.threadMessage("Finally!"); } }
```

## Rulare `MessageLoopInterrupted`

```
main: Starting MessageLoop thread
main: Waiting for MessageLoop thread to finish
main: Still waiting...
main: Still waiting...
Thread-0: This
main: Still waiting...
main: Still waiting...
Thread-0: is
main: Still waiting...
main: Tired of waiting!
Thread-0: I wasn't done!
main: Finally!
```

## `ThreadLocal`: variables locat to the thread

```java
public class ThreadLocalId implements Runnable {
    private ThreadLocal<Long> threadLocal = new ThreadLocal<>();

    public void run() {
      threadLocal.set(Thread.currentThread().getId());
      System.out.format("Name: %s Id: %d%n",
          Thread.currentThread().getName(), threadLocal.get());
    }

  public static void main(String[] args) throws InterruptedException {
    ThreadLocalId sharedRunnable = new ThreadLocalId();
    Thread thread1 = new Thread(sharedRunnable);
    Thread thread2 = new Thread(sharedRunnable);
    thread1.start(); thread2.start();
    thread1.join(); thread2.join();
  }
}
```

## Thread interference

```java
public class Interference {
  static int c = 0;
  public static void main(String[] args)
      throws InterruptedException {
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) c++;
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x) c--;
    myThread.join();
    System.out.println("c = " + c);
  }
}
```

. . .

### `++` and `--` are not __atomic__

```
c = 2343
```

## Thread syncronization

### Synchronized methods

```java
private synchronized void syncMethod () {
    //method body
  }
```

### Synchronized code

```java
synchronized (object reference){ 
    // code
  }
```

### Synchonized methods are synchronizing the body using  `this`{.java}

```java
private void syncMethod () {
      synchronized (this){
         //method body
   }
}
```

# Thread synchronization mechanism

## Intrinsic locks
- Every object has an intrinsic lock associated with it

- A thread needing to access an object's fields should
  + `acquire` the objects intrinsic lock
  + access/alter the object's data
  + `release` the intrinsic lock
- No thread can acquire a lock while another one holds it
  + a thread attempting to do so will _block_ in the acquire phase



## When calling a synchronized method

### Non-static synchronized methods
- The intrinsic lock of the object is acquired
- The method is executed
- The intrinsic lock of the object is released
- no other non-static, syncronized methods can be called
  simultaneously __on the same object__

### Static synchronized methods
- Static methods use the Class object for that class
- No other static synchronized methods __belonging to the same
  class__ can be called simultaneously

## Warnings
- Access to the non-synchronized methods is not blocked
- Static and non-static synchronized methods do not mutually
  exclude each-other
- A thread can re-aquire a lock it is already holding
  (reentrant synchronization)
- `Thread.sleep()`{.java} does not release the locks
- `ob.wait()`{.java}  releases the intrinsic lock of `ob` held by the thread

## Solving interference by synchronization (on statements)

```java
public class NonInterference {
  static int c = 0;
  static Object cLock = new Object();
  public static void main(String[] args)
      throws InterruptedException {
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x)
        synchronized (cLock) { c++; }
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x)
      synchronized (cLock) { c--; }
    myThread.join();
    System.out.println("c = " + c);
  }
}
```

## Solving interference through synchronized methods

```java
public class SynchronizedMethod implements Runnable {
  private int c = 0;
  public static void main(String[] args)
      throws InterruptedException {
    SynchronizedMethod sm = new SynchronizedMethod();
    Thread t1 = new Thread(sm); Thread t2 = new Thread(sm);
    t1.start(); t2.start(); t1.join(); t2.join();
    System.out.println("c = " + sm.c);
  }

  @Override public void run() {
    for (int x = 0; x < 5000; ++x) incrementC();
  }

  synchronized void incrementC() { c++; }
}
```

## Lock properties

- Only one thread can hold a given lock at any given time

- A thread holds the intrinsil lock of an object if either
    - it executes a synchronized method of the object
    - it executes a block synchronized by the object
    - if the object's type is `Class`{.java}, and the thead executes a `static synchronized`{.java} method


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

