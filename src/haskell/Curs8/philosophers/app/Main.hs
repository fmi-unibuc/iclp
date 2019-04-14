module Main where

import           Control.Concurrent
import qualified Control.Monad      as Monad

import           Fork
import           Philosopher

philNames :: [String]
philNames = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main :: IO ()
main = do
    forks <- Monad.replicateM 5 newFork
    let philosophers = zipWith3 newPhilosopher philNames forks (tail forks ++ [head forks])
    putStrLn "Running the philosophers. Press enter to quit"
    Monad.mapM_ (forkIO . runPhilosopher) philosophers
    Monad.void getLine

