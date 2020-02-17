---
title: Implementarea Concurenței în limbaje de programare
subtitle: Introducere
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Motivație

## Dining Philosophers Problem [Dijkstra 1965, Hoare 1985]\ [^philCitation]

\small
> In ancient times, a wealthy philanthropist endowed a College to accommodate
> five eminent philosophers.
> Each philosopher had a room in which he could engage in his professional
> activity of thinking; there was also a common dining room, furnished with a
> circular table, surrounded by five chairs, each labelled by the name of the
> philosopher who was to sit in it.
> They sat anticlockwise around the table.
> To the left of each philosopher there was laid a golden fork, and in the centre
> stood a large bowl of spaghetti, which was constantly replenished.
> A philosopher was expected to spend most of his time thinking; but when he felt
> hungry, he went to the dining room, sat down in his own chair, picked up his
> own fork on his left, and plunged it into the spaghetti.
> But such is the tangled nature of spaghetti that a second fork is required to
> carry it to the mouth.
> The philosopher therefore had also to pick up the fork on his right.
> When we was finished he would put down both his forks, get up from his chair,
> and continue thinking.
> Of course, a fork can be used by only one philosopher at a time.
> If the other philosopher wants it, he just has to wait until the fork is
> available again.

[^philCitation]: See also: C.A.R. Hoare, Communicating Sequential Processes, 2004


## Dinining Philosophers Problem (picture) [^phil]

![](https://upload.wikimedia.org/wikipedia/commons/thumb/7/7b/An_illustration_of_the_dining_philosophers_problem.png/578px-An_illustration_of_the_dining_philosophers_problem.png){ height=75% }

[^phil]: Source: Wikipedia

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

## Concurrency and parallelism

> In the computer world, when we talk about concurrency, we refer to a series of
> independent and unrelated tasks that run simultaneously on a computer.
> This simultaneity can be real if the computer has more than one processor or a
> multi-core processor, or it can be apparent if the computer has only one core
> processor.
> 
> Another concept related to concurrency is parallelism.
> There are different definitions and relations with the concurrency concept.
> Some authors talk about concurrency when you execute your application with
> multiple threads in a single-core processor.
> With this, you can see when your program execution is apparent.
> They talk about parallelism when you execute your application with multiple
> threads in a multi-core processor or in a computer with more than one
> processor.

(Gonzalez, Javier Fernandez. Java 9 Concurrency Cookbook - Second Edition, 2017)

## Concurrency and parallelism (picture) [^concPar]


![](https://i.stack.imgur.com/mUlNV.jpg)

[^concPar]: Introduction to Concurrency in Programming Languages
    Matthew J. Sottile, Timothy G. Mattson, Craig E Rasmussen
    CRC Press, Sep 28, 2009


## Programarea aplicațiilor concurente

- Programele concurente descriu comportamente cu multe interacțiuni:
  + între utilizatorii aceleiași interfețe,
  + intre subsistemele aceluiași sistem,
  + între sisteme diferite.


- Scrise în limbaje de programare care oferă
  primitive pentru realizarea acestor interacțiuni.

- Comportament independent de sistemul de operare.
  + Dar, totuși, sistemul de operare poate oferi suport pentru obținerea acestui comportament.

## Modele de concurență (plan de curs)

- Fire de execuție și memorie partajată
  + în limbajul Java (3 săptămâni)
- Memorie tranzactională (STM)
  + în limbajul C++ (1 saptămână)
- Actori și comunicare bazată pe mesaje
  + în limbajele Erlang/Elixir (4 săptămâni)
- Apeluri asincrone (Futures)
  + în limbajele Javascript/Python (2 săptămâni)
- Programare bazată pe evenimente 
  + în limbajul Javascript (1 saptămână)
- Alte limbaje/paradigme (prezentările voastre)

## Laborator (dl. Adrian Budău)

- Exerciții pentru aprofundarea noțiunilor discutate la curs
- Teme pentru acasă (fiecare temă valoareaza 20 de puncte)
  + Vor fi 3 teme, acoperind cele 3 mari teme discutate la curs
  + Termenul de predare al unei teme: 4 săptămâni
  + Temele trebuie prezentate în persoană
  + Fiecare zi (lucrătoare) de întârziere se penalizează cu 1 punct

## Proiect (valorează 40 de puncte)

+ Proiectele sunt individuale
+ Vor consta în 
   realizarea unei aplicații mai complexe care să folosească
    mai multe din modelele discutate la curs
+ __SAU__ într-un proiect de cercetare:
  * alegerea unei teme/articol de cercetare din listă (sau cu aprobare)
  * înțelegerea temei și realizarea unei prezentări și a unui demo
  * susținerea temei în cadrul unuia dintre cursurile finale
  * număr limitat
+ Proiectul trebuie ales până la vacanța de Paște
+ Termen predare: data examenului din sesiune
  * predarea mai devreme e binevenită și încurajată

## Condiții de promovabilitate 

- Nota finală = (NotaProiect + suma(noteTeme))/10
- Nota de trecere: 5
- Nota se rotunjește doar dacă este mai mare decât 5
  * i.e., nota 4,9 nu este notă de trecere

