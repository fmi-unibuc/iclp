---
title: Implementarea Concurenței în limbaje de programare
subtitle: Software transactional memory in C++
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Threads and synchronization in C++

## Hello threads `01-basic.cc`

- You can create and start a thread by creating a `thread` object

    ```cpp
    auto myThread = thread([]{cout << "Hello!"; });
    ```
- You can wait for a thread to finish using

    ```cpp
    myThread.join();
    ```


## Thread Interference (data races) `02-interference.cc`

### Data race
- Simultaneous conflicting accesses to the same memory location
- Considered undefined behavior
  + compiler can do unexpected optimizaitons

## Solving interference using mutexes and lock_guards `03-lock_guard.cc`
- Mutexes help secure critical sections

    ```cpp
    std::mutex g_i_mutex;
    ```
- Lock guards acquire the mutex in the constructor

    ```cpp
    const std::lock_guard<std::mutex> lock(g_i_mutex);
    ```
- ... and release it in the destructor
  + i.e. at the end of the enclosing block.

# The Bank problem

## Bank operations `04-account.hh`

- An account has a `name` and a `balance`
- To __deposit__ _amount_ into _A_
  + increase _A_'s _balance_ by _amount_
- To __withdraw__ _amount_ from _A_
  + check that _A_'s _balance_ is greater than _amount_
  + if so, then substract _amount_ from _A_'s _balance_
- To __transfer__ _amount_ from account _A_ to account _B_
  + attempt to __withdraw__ _amount_ from _A_
  + if above succeeded, __deposit__ _amount_ into _B_

## Concurrency anomalies at the Bank

Concurrent withdrawal
: might get more money than available `04-withdrawx2.cc`
: _Solution:_ synchronize __withdraw__

Concurrent deposit
: might get balance less than expected `04-depositxn.cc`
: _Solution:_ synchronize __deposit__

Concurrent transfer in the presence of failures
: might withdraw and fail before depositing `04-transfer.cc`
: _Solution:_ use transactions

# Software Transactional Memory

## Transactions and the __ACID__ model

- sequence of operations performed as a single unit of work
- Must exibit four properties (ACID model)
    
    Atomicity
    : either all operations are performed, or none
    
    Consistency
    : When completed, must leave data in a consistent state. 
    
    Isolation 
    : Modifications must happen as-if there are no other concurrent transactions
    
    Durability
    : Effects of a transaction must be persistent (even in presence of system failure)


## Software Transactional Memory

- Guarantees Atomicity and Isolation

Atomicity
: the effects of atomically act become visible to another thread all at once

Isolation
: the action is completely unaffected by other threads.
: It is as if act takes a snapshot of the state of the world 
  when it begins running, and then executes against that snapshot.

Programer Model
: You can think of it as if holding a global lock, but faster
: Not leaving the state inconsistent upon failure

## STM ---  Local (optimistic) execution
- Use a thread-local transaction log
- Each call to write writes the location its new value into the log
  + does not write to the location itself
- Each call to read first searches the log
  + if found, uses last value from log
  + if not, value is read from memory and recorded into the log

## STM --- finalizing a transaction

### Commiting a transaction

- Validating the log
  + each location read recorded in log must still hold the same value
- Commiting the log
  + commits all writes from the log to the memory (atomically)

### Retrying a transaction
- If validation fails ...
  + the transaction has an inconsistent view of the memory
- ... then abort transaction, reinitialize log, and retry

## STM in C++-17

C++-17 introduces 4 transactional statements

`synchronized { ... }`
: Executes as-if under a global lock.
: Allows non-transactional calls

Atomic statements
: limit the types of functions callable within
: must be `transaction_safe`

`atomic_noexcept { ... }`
: If an exception is thrown, `std::abort`{.cpp} is called

`atomic_cancel { ... }`
: If an exception is thrown, rolls-back transaction, then rethrows

`atomic_commit { ... }`
: If an exception is thrown, commit as-is

# Dining Problem

## Dining Philosophers Problem

> Five philosophers, numbered from 0 through 4 are living in a house where the
> table [is] laid for them, each philosopher having his own place at the table:

> ![](http://www.cs.utexas.edu/users/EWD/transcriptions/EWD03xx/EWD310_img_51.jpg){ height=30% }
> Their only problem —besides those of philosophy— is that the dish served is
> a very difficult kind of spaghetti, that has to be eaten with two forks.
> There are two forks next to each plate, so that presents no difficulty:
> as a consequence, however, no two neighbours may be eating simultaneously.

## Solution 1 `09-philosophers.cc`

- Take forks as a transaction, if they are available
  - if not, skip a meal

## Solution 2 `10-philosophers-locks.cc`

- Take both forks simultaneously
  - wait until both become available

## Solution 3 (do it yourselves)

- Take forks as a transaction, if they are available
- if not
  + abort transaction
  + wait for things to change
  + retry

- Talk with Adrian during lab-time about possible approaches to that