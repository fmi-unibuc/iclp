module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Monad            as Monad

import           ForkTMVar
import           PhilosopherTMVar

philNames :: [String]
philNames = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main :: IO ()
main = do
    forks <- Monad.mapM newFork [1..5]
    let philosophers = zipWith3 newPhilosopher philNames forks (tail forks ++ [head forks])
    putStrLn "Running the philosophers. Press enter to quit\n\n"
    asyncs <- Monad.mapM (async . finiteRunPhilosopher 3) philosophers
    race_ (mapM_ wait asyncs) (Monad.void getLine)

