---
title: Implementarea Concurenței în limbaje de programare
subtitle: Concurrency in Python
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Threads in Python

## Multithreading in Python [^1]

[^1]: Code samples taken from or inspired by 
  _Python Parallel Programming Cookbook_, by Giancarlo Zaccone

- Global Interpreter Lock: only one thread executes bytecode at a time
  - Hence, similar to concurrency on a single CPU

- However, GIL is released when performing I/O
  - Hence, multithreading can performance if threads contain I/O requests

- Shared Memory Model/API --- similar to Java/C++
  - using the `threading` module


## Thread-related classes

Thread

: class for threads being executed

Lock

: not reentrant locks
: can be released by other threads

RLock

: reentrant locks
: must be released by the same thread

Condition

: monitors with `wait` / `notify` mechanism

Sempaphore

: same as in Java

Event

: flag setting with wait conditions.

Barrier

: synchronizing multiple threads

Timer

: Delayed threads


## Threads --- Starting a thread directly

:::::: {.cell .code}
```python
import threading
t = threading.Thread(target=lambda : print("Hello "))
print("Greeting: ")
t.start()
print(" World!")
t.join()
```
::::::


### Thread arguments


target

: Function to be run by thread

args

: arguments (tupple) to be passed to `target`

kwargs

: keyword arguments to be passed to `target`

name

: a name for the thread

daemon

: whether the interpretor should wait for it to finish

## Threads --- Deriving the `Thread` class

:::::: {.cell .code}
```python
import threading
class myThread (threading.Thread):
  def __init__(self, what):
    threading.Thread.__init__(self)
    self.what = what
  def run(self):
    print (self.what)

thread1 = myThread("Hello ")
thread2 = myThread("World!")
print("Greeting: ")
thread1.start()
thread2.start()
```
::::::

### Important

`threading.Thread.__init__(self)`{.py} must be called from constructor

## Managing synchronization using `with`{.python} blocks

- with statements declare contexts protecting a resource
- resource is automatically *acquire*d when entering block
- resource is automatically *release*d when exiting block


:::::: {.cell .code}
```python
import threading
import logging

def threading_with(syncObject):
    with syncObject:
        logging.debug('%s acquired via with'  %syncObject)
        
def threading_not_with(syncObject):
    syncObject.acquire()
    try:
        logging.debug('%s acquired directly' %syncObject )
    finally:
        syncObject.release()


if __name__ == '__main__':

    logging.basicConfig(level=logging.DEBUG,
                    format='(%(threadName)-10s) %(message)s',)

    #let's create a test battery
    lock = threading.Lock()
    rlock = threading.RLock()
    condition = threading.Condition()
    mutex = threading.Semaphore(1)
    threading_synchronization_list = [lock ,rlock , condition , mutex]

    #in the for cycle we call the threading_with e threading_no_with function
    for statement in threading_synchronization_list :
        t1 = threading.Thread(target=threading_with, args=(statement,))
        t2 = threading.Thread(target=threading_not_with, args=(statement,))
        t1.start()
        t2.start()
        t1.join()
        t2.join()
```
::::::

## Condition

- Similar to Condition objects in Java 
- internal lock must be *acquire*d before calling `wait`
- Can create multiple conditions for same lock
  - `c = threading.Condition(lock)`{.py}
- Convenience method for waiting for a predicate
  - `c.wait_for(lambda : items > 2)`{.py}

:::::: {.cell .code .notes}
```python
from threading import Thread, RLock, Condition
import time
import random

class dropbox:  # for demo; better implementation in `queue` module
  def __init__(self):
    self.items = []
    self.lock = RLock()
    self.producer = Condition(self.lock)
    self.consumer = Condition(self.lock)
    self.closed = False
  
  def put(self, item):
    with self.lock:
      self.producer.wait_for(lambda: len(self.items) < 10 or self.closed)
      if (self.closed):
        return False
      self.items.append(item)
      self.consumer.notify()
      return True

  def take(self):
    with self.lock:
      self.consumer.wait_for(lambda: len(self.items) > 0 or self.closed)
      if (self.closed):
        return None
      item = self.items.pop()
      self.producer.notify()
      return item
  
  def close(self):
    with self.lock:
      self.closed = True
      self.consumer.notifyAll()
      self.producer.notifyAll()

class consumer(Thread):
  def __init__(self, id, box):
    Thread.__init__(self)
    self.id = id
    self.box = box

  def run(self):
    while True:
      item = self.box.take()
      if item is None:
        return
      logging.debug("Consumer" + str(self.id) + " got: " + item)
      time.sleep(random.random())
      logging.debug("Consumer" + str(self.id) + " consumed: " + item)

class producer(Thread):
  def __init__(self, id, box):
    Thread.__init__(self)
    self.id = id
    self.box = box
    self.count = 0

  def run(self):
    while True:
      self.count = self.count + 1
      time.sleep(2*random.random())
      item = "P"+str(self.id)+"-"+str(self.count)
      logging.debug("Producer" + str(self.id) + " produced: " + item)
      if not self.box.put(item):
        return
      logging.debug("Producer" + str(self.id) + " put: " + item)

if __name__ == "__main__":
  logging.basicConfig(level=logging.DEBUG, format='%(message)s',)
  box = dropbox()
  producer1 = producer(1, box)
  producer2 = producer(2, box)
  consumer1 = consumer(1, box)
  consumer2 = consumer(2, box)
  producer1.start()
  consumer1.start()
  producer2.start()
  consumer2.start()
  time.sleep(10)
  print("Closing the box")
  box.close()
  producer1.join()
  producer2.join()
  consumer1.join()
  consumer2.join()
  print("All threads done")
```
::::::


## Timer

- Subclass of Thread
- Delayed start
- Can be canceled (if not yet started)
  - Using the `cancel()` method

:::::: {.cell .code}
```python
from threading import Timer
def hello():
    print("hello, world")

t = Timer(3.0, hello)
t.start()  # after 3 seconds, "hello, world" will be printed
```
::::::


# Processes in Python

## Summary

- Multiple execution threads (processes)
  - using the `multiprocess` module
  - avoids the GIL by using subprocesses
  - takes advantage of the hardware multithreading

- provides the same synchronization mechanisms as for threads
  - all APIs from `threading`

- and more
  - `Queue` and `Pipe` for communication
  - `Manager` for managing shared resources through `Proxy` objects
  - `Pool` for data parallelims (e.g., map)

## Process synchronization example

:::::: {.cell .code}
```python
from multiprocessing import Process, Lock

def f(l, i):
  time.sleep(1)
  with l:
    print('hello ', i)
    print('world ', i)

if __name__ == '__main__':
  lock = Lock()
  for num in range(10):
    Process(target=f, args=(lock, num)).start()
```
::::::


## Pipe

:::::: {.cell .code}
```python
import time, random
from multiprocessing import Process, Pipe, current_process
from multiprocessing.connection import wait

def foo(w):
  for i in range(10):
    w.send((i, current_process().name))
  w.close()

if __name__ == '__main__':
  readers = []

  for i in range(4):
    r, w = Pipe(duplex=False)
    readers.append(r)
    p = Process(target=foo, args=(w,))
    p.start()
    w.close()

  while readers:
    for r in wait(readers):
      try:
        msg = r.recv()
      except EOFError:
        readers.remove(r)
      else:
        print(msg)
```
::::::

## Value

- Shared memory between processes

:::::: {.cell .code}
```python
from multiprocessing import Process, Value

def inc(x):
    with x.get_lock():
        x.value += 1

if __name__ == '__main__':
    #mgr = multiprocessing.Manager()
    x = Value('i', 0)
    jobs = [ Process(target=inc, args=(x,))
             for i in range(100) 
           ]
    for j in jobs:
        j.start()
    for j in jobs:
        j.join()
    print ('Result:', x.value)
```
::::::

## Pool

- Data parallelism using multiple subprocesses

:::::: {.cell .code}
```python
#Using a Process Pool – Chapter 3: Process Based Parallelism
import multiprocessing

def function_square(data):
    result = data*data
    return result


if __name__ == '__main__':
    inputs = list(range(0,100))
    pool = multiprocessing.Pool(processes=4)
    pool_outputs = pool.map(function_square, inputs)

    pool.close() 
    pool.join()  
    print ('Pool    :', pool_outputs)
```
::::::

# Asynchronous computations in Python

## Executors and pools

### ThreadPoolExecutor

- executes tasks using a thread pool
- not very useful if tasks mainly computational
- good if tasks partly I/O

### ProcessPoolExecutor

- executes tasks using a process pool
- takes advantage of hardware threads

:::::: {.cell .code .notes}
```python
##Concurrent.Futures Pooling - Chapter 4 Asynchronous Programming

import concurrent.futures
import time

def clock():
  return time.clock_gettime(time.CLOCK_MONOTONIC)

number_list = [1,2,3,4,5,6,7,8,9,10]

def evaluate_item(x):
    #count...just to make an operation   
    result_item = count(x)
    #print the input item and the result
    print ("item " + str(x) + " result " + str(result_item))

def count(number) : 
    for i in range(0,10000000):
        i=i+1
    return i*number
 
if __name__ == "__main__":
    ##Sequential Execution
    start_time = clock()
    for item in number_list:
        evaluate_item(item)
    print ("Sequential execution in " + \
           str(clock() - start_time), "seconds")

    ##Thread pool Execution
    start_time_1 = clock()
    with concurrent.futures.ThreadPoolExecutor(max_workers=5)\
         as executor:
        for item in number_list:
            executor.submit(evaluate_item, item)
    print ("Thread pool execution in " + \
           str(clock() - start_time_1), "seconds")

    ##Process pool Execution
    start_time_2 = clock()
    with concurrent.futures.ProcessPoolExecutor(max_workers=5)\
         as executor:
        for item in number_list:
            executor.submit(evaluate_item, item)
    print ("Process pool execution in " + \
           str(clock() - start_time_2), "seconds")

```
::::::

## AsyncIO Summary

- Similar to events/callbacks from Javascript 

- Event loop
  - dispatches events to their handlers (callbacks)

- Coroutines
  - imperative style for callbacks
  - similar to Javascript's async functions

- Futures
  - asynchronous tasks

## Event loop

- `loop = get_event_loop()` --- gets current context's loop
- `loop.call_later(delay, callback, argument)`
- `loop.call_soon(delay, callback, argument)`
  - adds an event for calling the `callback` to the loop
- `loop.time()` --- gets the `loop` internal clock's time
- `loop.run_forever()` --- starts the loop
  - runs until `loop.stop()` is called

:::::: {.cell .code .notes}
```python
#
# Asyncio.loop - Chapter 4 Asynchronous Programming
#


import asyncio
import datetime
import time

def function_1(end_time, loop):
    print ("function_1 called at ", loop.time())
    if (loop.time() + 1.0) < end_time:
        loop.call_later(1, function_2, end_time, loop)

def function_2(end_time, loop):
    print ("function_2 called at ", loop.time())
    if (loop.time() + 1.0) < end_time:
        loop.call_later(1, function_3, end_time, loop)

def function_3(end_time, loop):
    print ("function_3 called at ", loop.time())
    if (loop.time() + 1.0) < end_time:
        loop.call_later(1, function_1, end_time, loop)

def function_4(end_time, loop):
    print ("function_4 called at ", loop.time())
    if (loop.time() + 1.0) < end_time:
        loop.call_later(1, function_4, end_time, loop)

if __name__ == "__main__":
  loop = asyncio.get_event_loop()
  
  # Schedule the first call to display_date()
  end_loop_1 = loop.time() + 9.0
  print ("End time: ", end_loop_1)
  loop.call_soon(function_1, end_loop_1, loop)
  #loop.call_soon(function_4, end_loop_1, loop)
```
::::::

## Coroutines

:::::: {.cell .code .notes}
```python
#Asyncio Finite State Machine

import asyncio
import time
from random import randint

async def StartState():
    print ("Start State called \n")
    input_value = randint(0,1)
    time.sleep(1)
    if (input_value == 0):
        result = await State2(input_value)
    else :
        result = await State1(input_value)
    print("Resume of the Transition : \nStart State calling "\
          + result)
    
    
async def State1(transition_value):
    outputValue =  str(("State 1 with transition value = %s \n"\
                        %(transition_value)))
    input_value = randint(0,1)
    time.sleep(1)
    print("...Evaluating...")
    if (input_value == 0):
        result =  await State3(input_value)
    else :
        result = await State2(input_value)
    result = "State 1 calling " + result
    return (outputValue + str(result))


async def State2(transition_value):
    outputValue =  str(("State 2 with transition value = %s \n" \
                        %(transition_value)))
    input_value = randint(0,1)
    time.sleep(1)
    print("...Evaluating...")
    if (input_value == 0):
        result = await State1(input_value)
    else :
        result = await State3(input_value)
    result = "State 2 calling " + result
    return (outputValue + str(result))

async def State3(transition_value):
    outputValue =  str(("State 3 with transition value = %s \n" \
                        %(transition_value)))
    input_value = randint(0,1)
    time.sleep(1)
    print("...Evaluating...")
    if (input_value == 0):
        result = await State1(input_value)
    else :
        result = await EndState(input_value)
    result = "State 3 calling " + result
    return (outputValue + str(result))


async def EndState(transition_value):
    outputValue =  str(("End State with transition value = %s \n"\
                        %(transition_value)))
    print("...Stop Computation...")
    return (outputValue )

if __name__ == "__main__":
    print("Finite State Machine simulation with Asyncio Coroutine")
    loop = asyncio.get_event_loop()
    loop.create_task(StartState())
```
::::::
