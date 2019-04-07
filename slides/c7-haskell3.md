---
title: Implementarea concurenței în limbaje de programare
subtitle: Haskell 2 --- MVar
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
thanks: Bazat pe cursul ținut de dna. prof. Ioana Leuștean
abstract: |
---

# Variabile Mutabile

## [`Control.Concurrent.MVar t`{.hs}](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html)

### "Concurrent Haskell" by S. Peyton Jones, A. Gordon, and S. Finne

`MVar t` e o locație modificabilă care e goală sau conține o valoare de tip `t`.

### Operații de bază

`newEmptyMVar :: IO (MVar a)`{.hs}

: crează o nouă locație `MVar`, inițial goală

`newMVar :: a -> IO (MVar a)`{.hs}

: crează o nouă locație `MVar`, și pune valoarea dată

`putMVar :: MVar a -> a -> IO ()`{.hs}

: pune valoare în locație dacă e goală; se blochează daca nu

`takeMVar :: MVar a -> IO a`{.hs}

: scoate valoarea din MVar daca e plină; se blochează daca nu

## Benjamin Kovach, [Haskell Bits #6 - A Guide to Mutable References](https://www.kovach.me/posts/2017-06-22-mutable-references.html)

![MVar e un canal cu dimensiune 1](https://www.kovach.me/images/HaskellRefs/mvar.png){height=50%}


## MVar – Moduri de folosire

* Locații de memorie sincronizate
    - `putMVar` scrie, `takeMVar` citește
* Canale de comunicare
    - `putMVar` trimite, `takeMVar` primește
* Semafor binar
    - `putMVar` e `signal`, `takeMVar` e `wait`


## `takeMVar` și `putMVar`

* Sunt operații blocante (dacă variabila `MVar` e goală, respectiv plină)

* Sunt _single-wakeup_
    * Un singur thread (dacă există) va fi trezit când operația devine posibilă

* Thread-urile așteaptă într-o coadă
    * pentru aceeași operație la aceași `MVar`
    * vor fi trezite pe rând

## Haskell detectează deadlocks

```hs
import           Control.Concurrent

main :: IO ()
main = do
    m <- newEmptyMVar
    takeMVar m
```

```
*Main> main
*** Exception: thread blocked indefinitely in an MVar operation
*Main>
```

## Alte funcții

`readMVar :: MVar a -> IO a`{.hs}

: Citește conținutul unei `MVar`

    * operație atomică
    * se blochează dacă `MVar` e goală
    * _multiple-wakeup_ toate thread-urile care așteaptă vor primi aceeași valoare
    * Nu golește locația `MVar`

`tryTakeMVar :: MVar a -> IO (Maybe a)`{.hs}

: Ia valoarea din `MVar`

    * doar dacă locația `MVar` e plină
    * nu se blochează
    * dacă `MVar` e goală întoarce `Nothing`

`tryPutMVar :: MVar a -> a -> IO Bool`{.hs}

: Pune valoarea în `MVar`
    
    * doar dacă locația `MVar` e goală
    * nu se blochează
    * dacă `MVar` e plină întoarce `False`

# Canale de comunicare folosind `MVar`

## Ideea de canal de comunicare

```hs
module Channel (Channel, newChannel, send, receive) where

import           Control.Concurrent

class Channel channel where
    newChannel :: IO (channel a)
    send :: channel a -> a -> IO ()
    receive :: channel a -> IO a
```

Pentru a putea comunica, trebuie să putem să

* creem un canal nou de comunicare
* să trimitem un mesaj
* să primim un mesaj

## Exemplu: Problema producător-consumator 


```hs
module ProducerConsumer (produce, consume) where
import           Channel
import           Control.Monad (forever, replicateM_)

produce
    :: Channel channel
    => channel a -> IO a -> IO ()
produce channel pAction = forever $ do  msg <- pAction
                                        send channel msg
consume
    :: Channel channel
    => channel a -> (a -> IO ()) -> Int -> IO ()
consume channel cAction size =
    replicateM_ size $ do   msg <- receive channel
                            cAction msg
```


## Canal de comunicare simplu folosind `MVar`

Folosim `MVar` pentru a avea un canal de comunicare (blocant) de dimensiune 1.

```hs
module BlockingChannel (BlockingChannel) where

import           Channel
import           Control.Concurrent

newtype BlockingChannel a =
    BlockingChannel { getChannel :: MVar a }
    
instance Channel BlockingChannel where
    newChannel = BlockingChannel <$> newEmptyMVar
    send = putMVar . getChannel
    receive = takeMVar . getChannel
```

## Exemplu: `ProducerConsumerTest.hs`

```hs
import           BlockingChannel
import           Channel
import           Control.Concurrent (forkIO)
import           ProducerConsumer

newBlockingChannel :: IO (BlockingChannel String)
newBlockingChannel = newChannel
    
main :: IO ()
main = do
    channel <- newBlockingChannel
    forkIO (produce channel getLine)
    consume channel putStrLn 10
```

## Spawn și Join folosind `BlockingChannel`

```hs
module SpawnJoin (TId, spawn, join) where
import           BlockingChannel
import           Channel
import           Control.Concurrent

newtype TId = TId (BlockingChannel ())

spawn :: IO () -> IO TId
spawn action = do   done <- newChannel
                    forkIO $ do action
                                send done ()
                    return $ TId done
    
join :: TId -> IO ()
join (TId done) = receive done
```

## Exemplu cu `spawn` și `join`: NonInterference

```hs
import           Control.Monad (replicateM_)   
import           Data.IORef
import           SpawnJoin

work :: IORef a -> (a -> a) -> IO () 
work ref f = do replicateM_ 5800000
                    (atomicModifyIORef' ref (\x -> (f x, ())))
                putStrLn "done"
main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            th1Id <- spawn (work ref (+ 1))
            th2Id <- spawn (work ref (+ (-1)))
            join th1Id
            join th2Id
            val <- readIORef ref
            print val
```

## Rendezvous

```hs
module Rendezvous
  (Rendezvous, newRendezvous, leftShake, rightShake) where
import           Channel
data Rendezvous a b =
    Rendezvous { left :: Channel a, right :: Channel b }

newRendezvous :: IO (Rendezvous a b)
newRendezvous = Rendezvous <$> newChannel <*> newChannel

exchange :: Channel a -> Channel b -> a -> IO b
exchange channelA channelB a = do   send channelA a
                                    receive channelB
leftShake :: Rendezvous a b -> a -> IO b
leftShake rv = exchange (left rv) (right rv)
rightShake :: Rendezvous a b -> b -> IO a
rightShake rv = exchange (right rv) (left rv)
```

# Semafor binar folosind `MVar`

## Implementarea blocului `synchronized`{.java}

```hs
module Semaphore.Syncronized
    (Synchronized, newSynchronized, synchronized)
  where
import           Control.Concurrent

newtype Synchronized = Synchronized (MVar ())

newSynchronized :: IO Synchronized
newSynchronized = Synchronized <$> newMVar ()

synchronized :: Synchronized -> IO () -> IO ()
synchronized (Synchronized mvar) action = do
    takeMVar mvar
    action
    putMVar mvar ()
```

## Exemplu: Non-Interferență folosind `synchronized`

```hs
import           Control.Monad          (replicateM_)
import           Data.IORef
import           Semaphore.Synchronized
import           SpawnJoin
work :: IORef a -> (a -> a) -> Synchronized -> IO ()
work ref f mutex = replicateM_ 5800000 $
                synchronized mutex (modifyIORef' ref f)
main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            mutex <- newSynchronized
            th1Id <- spawn (work ref (+ 1) mutex)
            th2Id <- spawn (work ref (+ (-1)) mutex)
            join th1Id
            join th2Id
            val <- readIORef ref
            print val
```

# Variabilă sincronizată folosind `MVar`

## Exemplu: Non-Interferență folosind `MVar`

```hs
import           Control.Concurrent
import           Control.Monad      (replicateM_)
import           SpawnJoin

work :: MVar a -> (a -> a) -> IO ()
work ref f = replicateM_ 5800000 $ do   v <- takeMVar ref
                                        putMVar ref (f v)

main :: IO () 
main = do   ref <- newMVar (0 :: Integer)
            th1Id <- spawn (work ref (+ 1))
            th2Id <- spawn (work ref (+ (-1)))
            join th1Id
            join th2Id
            val <- readMVar ref
            print val
```

# Exemplu mai complex: canal nemărginit

## Descrierea problemei

### Cerințe.  Vrem un canal care

* este o instanță a clasei `Channel`
    * `newChannel`, `send`, `receive`
* Se blochează la citire dacă e gol
* nu se blochează la scriere
    * deci, are memorie nemărginită

. . .

### Idee: Implementăm o structură de date înlănțuită de tip coadă

* Un nod e format din valoare și legătură către următorul nod
* folosim `MVar` pentru
    * a reprezenta legăturile către nodul următor
    * a asigura sincronizarea
* santinele pentru prima și ultima poziție din coadă

## Descriere vizuală

Sursa: [Parallel and Concurrent Programming in Haskell by Simon Marlow](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch07.html#sec_channels)

![Structura implementării canalului nemărginit](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/httpatomoreillycomsourceoreillyimages1724958.png)

## Implementare: Structura de date

```hs
module NonBlockingChannel (NonBlockingChannel) where

import           Channel
import           Control.Concurrent

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)

data NonBlockingChannel a =
    NonBlockingChannel
    { readEnd  :: MVar (Stream a)
    , writeEnd :: MVar (Stream a) -- always empty
    }
```

## Implementare:  instanța clasei `Channel`

```hs
instance Channel NonBlockingChannel where
    newChannel = do
        stream <- newEmptyMVar
        return NonBlockingChannel
            <*> newMVar stream <*> newMVar stream
    send channel a = do
        lastCell <- takeMVar (writeEnd channel)
        lastCell' <- newEmptyMVar
        putMVar lastCell (Item a lastCell')
        putMVar (writeEnd channel) lastCell'
    receive channel = do
        firstCell <- takeMVar (readEnd channel)
        (Item a firstCell') <- takeMVar firstCell
        putMVar (readEnd channel) firstCell'
        return a
```
