---
title: Implementarea concurenței în limbaje de programare
subtitle: Haskell 1 --- concurență, thread-uri, memorie partajată
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
thanks: Bazat pe cursul ținut de dna. prof. Ioana Leuștean
abstract: |
---

## Simon Marlow: Parallel and Concurrent Programming in Haskell (Part II. Concurrent Haskell. Cap.7 & 8)

### Haskell oferă mai multe modele de concurență

"Haskell does not take a stance on which
concurrent programming model is best:
__actors, shared memory, and transactions__ are all
supported, for example." (S. Marlow)

### Programatorul trebuie să știe să aleagă modelul potrivit

"Haskell provides all of these concurrent programming
models and more - but this flexibility is a double-edged
sword. The advantage is that you can choose from a wide
range of tools and pick the one best suited to the task at
hand, but the disadvantage is that it can be hard to
decide which tool is best for the job." (S. Marlow)

## Thread-uri în Haskell

* Thread-urile au efecte laterale
    - interacționează cu lumea exterioară
    - deci, programarea concurentă în Haskell are loc în _monada_ `IO`

* Efectele thread-urilor sunt intercalate nedeterminist la rulare

* Thread-urile sunt create și gestionate intern
    - fără a folosi facilitățile sistemului de operare

* Implementarea asigură corectitudine (_fairness_)


# Pachetul de bază `Control.Concurrent`

## `forkIO :: IO () -> IO ThreadId`

```hs
import           Control.Concurrent

printChars :: [Char] -> IO ()
printChars = mapM_ putChar

main :: IO ()
main =  do
        forkIO (printChars ['a'..'z'])  --- thread-ul copil
        printChars ['A'..'Z']           --- thread-ul părinte
```
. . . 
```hs
*Main> :t mapM_
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
```
```
*Main> main
ABCDaEbFcGdHeIfJgKhLiMjNkOlPmQnRoSpTqUrVsWtXuYvZw
xyz
```

## Citate despre thread-uri

### S. Marlow, Parallel and Concurrent Programming in Haskell

The computation passed to `forkIO` is executed in a new thread that runs
concurrently with the other threads in the system. If the thread has effects,
those effects will be interleaved in an indeterminate fashion with the effects
from other threads.

### Simon Peyton Jones, A Gordon, S Finne, Concurrent Haskell

forkIO is assymetrical: when a process executes a forkIO it spawns a child
process that executes concurrently with the continued execution of the parent.

### B. O'Sullivan, D. Stewart, J. Goerzen, Real World Haskell

GHC's runtime system treats the program's original thread of control
differently from other threads.
When this thread finishes executing, the runtime system considers the program 
as a whole to have completed. 
If any other threads are executing at the time, they are terminated.

## Exemplu: Reminders (S. Marlow)

```hs
import           Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do   s <- getLine
            if s == "exit" then return ()
            else do forkIO (setReminder (read s))
                    main

setReminder :: Int -> IO ()
setReminder t = do
    putStrLn $
        "Reminder set for " ++ show t ++ " seconds."
    threadDelay $ t * 10^6
    putStrLn $
        "Reminder for " ++ show t ++ " seconds is up!\BEL"
```

# Locații de memorie în Haskell

## Modulul `Data.IORef`

`data IORef a`{.hs}

: O variabilă modificabilă în monada `IO` (pointer) care ține valori de tipul `a`.

### Accesori

`newIORef :: a -> IO (IORef a)`{.hs}

: crează o nouă locație de memorie, inițializată cu valoarea dată ca argument.

`readIORef :: IORef a -> IO a`{.hs}

: citește valoarea unei locații de memorie.

`writeIORef :: IORef a -> a -> IO ()`{.hs}

: setează valoarea unei locații de memorie.

`modifyIORef :: IORef a -> (a -> a) -> IO ()`{.hs}

: modifică valoarea unei locații de memorie conform funcției date

## Interferență cu `IORef`

```hs
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (replicateM_)
import           Data.IORef

work :: IORef a -> (a -> a) -> IO ()
work ref f = do replicateM_ 580000 (modifyIORef' ref f)
                putStrLn "done"

main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            forkIO (work ref (+ 1))
            forkIO (work ref (+ (-1)))
            threadDelay $ 1 * 10^6
            val <- readIORef ref
            print val
```

## Rezolvarea interferenței

```hs
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (replicateM_)
import           Data.IORef

work :: IORef a -> (a -> a) -> IO ()
work ref f = do replicateM_ 580000 $
                    atomicModifyIORef' ref (\x -> (f x, ()))
                putStrLn "done"
main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            forkIO (work ref (+ 1))
            forkIO (work ref (+ (-1)))
            threadDelay $ 1 * 10^6
            val <- readIORef ref
            print val
```

## Limitări ale lui `atomicModifyIORef` (citat din documentație)

`atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b`{.hs}

Atomically modifies the contents of an `IORef`.

This function is useful for using `IORef` in a safe way in a multithreaded
program. If you only have one `IORef`, then using `atomicModifyIORef` to access
and modify it will prevent race conditions.

Extending the atomicity to multiple `IORef`s is problematic, so it is
recommended that if you need to do anything more complicated, then using `MVar`
instead is a good idea.

### Observație

Rezultatul final al acțiunii este calculat în funcție de valoarea curentă
a locației de memorie. 
