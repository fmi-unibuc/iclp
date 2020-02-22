---
title: Implementarea Concurenței în limbaje de programare
subtitle: Intro
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Motivation

## Dining Philosophers Problem\ [^philCitation]


> Five philosophers, numbered from 0 through 4 are living in a house where the
> table [is] laid for them, each philosopher having his own place at the table:

> ![](http://www.cs.utexas.edu/users/EWD/transcriptions/EWD03xx/EWD310_img_51.jpg){ height=30% }
> Their only problem —besides those of philosophy— is that the dish served is
> a very difficult kind of spaghetti, that has to be eaten with two forks.
> There are two forks next to each plate, so that presents no difficulty:
> as a consequence, however, no two neighbours may be eating simultaneously.

[^philCitation]: [Hierarchical ordering of sequential processes](http://www.cs.utexas.edu/users/EWD/index03xx.html), EW Dijsktra, 1971


## The essence of concurrency [^essenceCitation]

> Imagine the following scenario:
>
> - Several cars want to drive from point A to point B.
>
> - They can compete for space on the same road and end up either
>   + following each other or
>   + competing for positions (and having accidents!).
>
> - Or they can drive on parallel lanes,
>   + thus arriving at about the same times
>   + without getting in each other's way.
>
> - Or they could travel different routes, using separate roads. 
>
> This scenario captures the essence of concurrent computing.

[^essenceCitation]: G.R. Andrews,  Foundations of Multithreaded, Parallel and Distributed
Programming, 2002

## The essence of concurrency (picture) [^essence]

![](https://drive-development.com/images/swiftLibrary/threadssandtasks.png){ height=75% }

[^essence]: Source: [drive-development.com](http://drive-development.com)

## Concurrency and parallelism (picture) [^concPar]


![](https://i.stack.imgur.com/mUlNV.jpg)

[^concPar]: Introduction to Concurrency in Programming Languages
    Matthew J. Sottile, Timothy G. Mattson, Craig E Rasmussen
    CRC Press, Sep 28, 2009


## Concurrent programming concepts

- Concurrent programs describe interactive behaviors
  + between the users of the same API
  + between subsistems of the same system
  + between different systems


- Programming languages offer primitives for achieving this level of interaction

- Behavior should be independent of the operating system
  + However, the OS may offer support for achieving the behavior.

## Course

- Lecturer: Traian Șerbănuță (traian.serbanuta at unibuc.ro)

### Timeline
- Threads and shared memory (using Java, 3 weeks)
  + Software transactional memory (using C++, 1 week)
- The actor model (using Erlang/Elixir, 4 weeks)
- Asynchronous calls (using Javascript/Python, 2 weeks)
  - Event based programming (using Javascript, 1 week)

## Lab

- Lab facilitator: Adrian Budău (budau.adi at gmail.com)

### Lab assignments
- Three assignments, covering the three main topics discussed
  + Each assignment accounts for 20 points
- Duration of an assignment: 4 weeks since it is posted.
- Assitance will be provided during lab hours
- Must be delivered and defended in-person during lab hours
- Delayed submission: *5 points of penalty for every week*

## Project (40 points)

+ Projects are individual
+ Deadline for choosing a project: before the Easter holiday

### Two avenues to success :-)
1. Building a more complex application
   - using more than one of the concurrency models discussed
   - Deadline: in session, by exam date
   - Early submissions are welcomed and appreciated
2. Research (for the more theoretically inclined)
   * Choosing a concurrency research paper (ask me for guidance)
   * Understanding the theme, creating a presentation and a demo
   * Presenting the findings to your peers during final classes
   * Deadline for submitting: *May 23* 

## Grading

- Final grade = (ProjectScore + sum(assignmentScores))/10
- Passing grade: 5
  * 4.9 is not a passing grade
- Passing grades will be rounded to the nearest whole number

