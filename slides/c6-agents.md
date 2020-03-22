---
title: Implementarea Concurenței în limbaje de programare
subtitle: Actor-based programming --- Erlang/Elixir
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Shared memory model limitations

## The problem with objects and threads (shared memory)

### Ilusion of encapsulation
![Object state can be corrupted in a multithreaded environment](https://doc.akka.io/docs/akka/2.5.2/scala/guide/diagrams/seq_chart_multi_thread.png){width=50%}

Solution
: Locks and other synchronization mechanisms.

## The problem with locks and synchronization mechanisms

- Locks seriously limit concurrency
  + very costly on modern CPU architectures
  + Heavy-lifting from the OS to suspend and restore threads
- All calling threads blocked until method finishes
  + Can be addressed by spwning separate threads, but it's costly
- Locks introduce the danger of deadlock

### No-win situation
- Without sufficient locks, the state gets corrupted.
- With many locks in place, performance suffers
  + and very easily leads to deadlocks.

## Ilusion of shared memory

- In multicore environments, there is no real shared-memory anymore
- Each CPU core maintains its own cache
- Synchronization between CPUs through messages (cache lines)
- Costly and inefficient to guaranteee the same view of memory

## Ilusion of a call stack

![Task delegation interferes with call stacks ](https://doc.akka.io/docs/akka/2.5.2/scala/guide/diagrams/exception_prop.png){width=75%}

# Actors


## The actor model

* Enforce encapsulation without resorting to locks.
* Progress achieved through cooperative entities
  + reacting to signals,
  + changing state
  + sending signals to each other
* Stop worrying about an executing mechanism

## Everybody is an actor!

> The actor model can be imagined as a world where everyone is sitting alone in
> their own room and can perform a few distinct tasks.
> Everyone communicates strictly by writing letters and that's it.
> While it sounds like a boring life (and a new age for the postal service), it
> means you can ask many people to perform very specific tasks for you, and none
> of them will ever do something wrong or make mistakes which will have
> repercussions on the work of others; they may not even know the existence of
> people other than you (and that's great).
>
> _Fred Hébert, [Learn You Some Erlang For Great Good](http://learnyousomeerlang.com/introduction#what-is-erlang)_


## Actors

### Definition
An actor is a computational entity that, in response to a message it receives, can concurrently:

- send a messages to other actors;
- create new actors;
- designate the behavior to be used for the next message it receives.

### Defining characteriristics
- Interaction only through asynchronous communication, using _message passing_
- Inherent concurrency of computation withing and among actors
- Private communication based on address
  - Agent can only communicate with
    - The agents it created (its children)
    - Agents of whom it found out through messages it received

## When an actor receives a message:


- Message gets added to the end of a queue (Mailbox).
- Actor picks the message from the front of the queue.
- Actor modifies internal state, sends messages to other actors.

![Actors passing messages](https://doc.akka.io/docs/akka/2.5.2/scala/guide/diagrams/serialized_timeline_invariants.png){width=40%}

## Problems solved by actors

Encapsulation
: by decoupling execution from signaling
: calls transfer execution, message passing doesn't

No need for locks
: Messages are processed one at a time, hence no races

Task delegation
: No locks means all threads can be fully used

Local state
: Changes and data propagated through messages
: Corresponds to how current architectures work
: Same model locally and in the network 

Error Handling
: Parents supervise children and may restart them
: Children notify parents when they fail
: Increased fault tolerance

# Erlang / Elixir

## Erlang [^name]

- Designed to program concurrent, distributed,
fault-tolerant, scalable, soft, real-time systems. 

- Processes __only__ interact by exchanging
messages.

- Processes share no data with other processes.

- Easily distributable over multicores or networks.

[^name]: Named for Danish mathematician Anger Krarup Erlang

### What runs on Erlan?

- Most Ericsson-based infrastructure: GPRS, 3G, 4G, 5G
  + including (Rom)Telekom
- CISCO ~ 90% of internet traffic is routed using Erlang
- WhatsApp

## Erlang language features

- Functional language
  + no mutable state
  + higher-order functions
  + dynamically typed

- Compiles to bytecode, runs on a VM called BEAM

### Open Telecom Platform (OTP)
  + collection of middleware, libraries and tools in Erlang
  + Supporting development in Erlang
  + APIs for building robuist distributed systems 

## Elixir

- Built on top of Erlang and running on the Erlang VM (BEAM)

- Combines best features of Ruby, Erlang, and Clojure

### Goals
- Keeping compatibility with the Erlang ecosystem (OTP)
- Enable higher extensibility and productivity in the Erlang VM

## Elixir improvements over Erlang

- Metaprogramming (through macros)
- A project and dependency management tool (named mix)
- A powerful unit testing framework
- Unicode strings and unicode operations
- More data structures
  - ranges
  - novel implementations for sets and dictionaries
- Polymorphic records
- Strict and lazy enumeration APIs
- Convenience functions for scripting, like working with paths and the filesystem
- Nicer syntax (subjective)

## Elixir resources

### Principal access point: [elixir-lang.org](http://elixir-lang.org/)
- Downloads
- Documentation
- Tutorials

### Asking for help
- [Official #elixir-lang on freenode IRC](irc://irc.freenode.net/elixir-lang)
- [Elixir Forum](http://elixirforum.com/)
- [Elixir on Slack](https://elixir-slackin.herokuapp.com/)
- [Elixir on Discord](https://discord.gg/elixir)
- [elixir tag on StackOverflow](https://stackoverflow.com/questions/tagged/elixir)


## Getting started

### Interactive interpreter --- iex

```
$ iex
Erlang/OTP 22 [erts-10.4.4] [source] [64-bit] 
[smp:8:8] [ds:8:8:10] [async-threads:1]

Interactive Elixir (1.9.1) - press Ctrl+C to exit 
(type h() ENTER for help)
iex(1)> 
```

### Scripts
- files with extension `.exs`
- can be loaded into the `iex` interpreter
- can be executed using `elixir`


## Datatypes (numbers)

```elixir
iex> is_integer(1)
true
iex> is_integer 0x1F
true
iex> is_integer ?ș
true
iex> is_number 1
true
iex> is_float 1.0
true
iex> is_number 3.14
true
```

## Datatypes (booleans and atoms)
```elixir
iex> is_boolean true
true
iex> is_atom :Salut
true
iex> is_atom :true
true
iex> is_boolean :true
true
iex> is_atom true
true
iex> is_atom :Șerbănuță
true
iex> is_atom Traian
true
```

## Datatypes (strings, lists, tuples)

```elixir
iex> is_bitstring "elixir"
true
iex> is_list 'elixir'
true
iex> is_list [1,2,3]
true
iex> is_tuple {1, 2, 3}
true
iex> is_list [1, :a, "da"]
true
iex> is_tuple {1, true, [1,2,3]}
true
iex> is_map %{:nume => "Traian", :vârsta => "bătrân"}
true
```

## Datatypes (functions)

```elixir
iex> is_function fn x,y -> x + y end
true
iex> is_function &(&1 + &2)
true
iex> is_function &is_function/1
true
iex> is_function &+/2
true
iex> is_function &List.last/1
true
iex> add = fn x,y -> x + y - 1 end
#Function<13.91303403/2 in :erl_eval.expr/5>
iex> add.(3,5)
7
```

## Lists

- Lists in square brackets, separated by `,` 
- Within the same list, elements' type can vary

```elixir
iex> [1, 2, true, 3]
[1, 2, true, 3]
iex> length [1, 2, 3]
3
```

### List append and difference

```elixir
iex> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex> [1, true, 2, false, 3, true] -- [true, false]
[1, 2, 3, true]
```

### Lists of characters

```elixir
iex> [104, 101, 108, 108, 111]
'hello'
```

## Boolean operators

### Strict boolean operators --- `not/1`{.elixir}, `or/2`{.elixir}, `and/2`{.elixir}
- first argument must be boolean

```elixir
iex> false or is_atom(:example)
true
iex> 1 and true
** (BadBooleanError) expected a boolean on left-side of "and", got: 1
```

### Non-strict boolean operators --- `!/1`{.elixir}, `||/2`{.elixir}, `&&/2`{.elixir} 
- everything except `false`{.elixir} and `nil`{.elixir} is true
```elixir
iex(45)> ! nil  
true
iex(46)> nil && false
nil
iex(47)> nil || false
false
iex(48)> nil || 1
1
```

## Comparison operators
```elixir
iex> 1 == 1
true
iex> 1 != 2
true
iex> 1 < 2
true
iex> 1 == 1.0
true
iex> 1 === 1.0
false
iex> 1 < :atom
true
```

- number < atom < reference < function < port < pid < tuple < map < list < bitstring

## Higher-order functions

```
iex> Enum.map [1,2,3,4,5], &(2 * &1)
[2, 4, 6, 8, 10]

iex> Enum.reduce 1..5, &+/2        
15
iex> Enum.reduce 1..5, &(&1 + 10 * &2)
12345
iex> Enum.reduce [1,2,3,4,5], [], &([&1] ++ &2)
[5, 4, 3, 2, 1]

iex> require Integer # to use macros from the Integer module
Integer
iex> Enum.filter 1..10, &Integer.is_odd/1
[1, 3, 5, 7, 9]
```

## Comprehensions

```
iex> for i <- [:a, :b, :c], j <- 1..10, rem(j, 3) == 0,
     do:  {i, j}
[a: 3, a: 6, a: 9, b: 3, b: 6, b: 9, c: 3, c: 6, c: 9]


iex> for <<c <- "Salut, sunt eu, un haiduc!">>, do: c
'Salut, sunt eu, un haiduc!'

iex> for <<c <- "Salut, sunt eu, un haiduc!">>,
     c != ?u, into: "", do: <<c>>
"Salt, snt e, n haidc!"
```

## Pattern matching basics

- Match on structure
```elixir
iex> {a, b, c} = {:hello, "world", 42}
{:hello, "world", 42}
iex> a
:hello
iex> b
"world"
```

- Head-tail list matching like in Prolog
```elixir
iex> [_, h2 | tail] = [1, 2, 3, 4]
[1, 2, 3, 4]
iex> h2
2
iex> tail
[3, 4]
```

## Pinning


### Multiple matchings lead to rebinding
```elixir
iex> x = 1
1
iex> x = 2
2
```

### Binding occurs only on the left-hand-side
```elixir
iex> x = 1
1
iex> 2 = x
** (MatchError) no match of right hand side value: 1
```

### Pinning operator `^`{.elixir}
```elixir
iex> x = 1
1
iex> ^x = 2
** (MatchError) no match of right hand side value: 2
```

## `case`{.elixir} with matching clauses and guards

```elixir
iex> case {1, 2, 3} do
...>   {4, 5, 6} ->
...>     "This clause won't match"
...>   {1, x, 3} when x > 0->
...>     "This clause will match and bind x to 2 in this clause"
...>   _ ->
...>     "This clause would match any value"
...> end
"This clause will match and bind x to 2 in this clause"
iex> x
2
```

## (Anonymous) functions with clauses and guards

```elixir
iex> f = fn
...>   x, y when x > 0 -> x + y
...>   x, y -> x * y
...> end
#Function<12.71889879/2 in :erl_eval.expr/5>
iex> f.(1, 3)
4
iex> f.(-1, 3)
-3
```