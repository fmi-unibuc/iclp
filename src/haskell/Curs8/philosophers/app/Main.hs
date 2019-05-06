module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Monad            as Monad

import           Fork
import           Philosopher

philNames :: [String]
philNames = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main :: IO ()
main = do
    forks <- Monad.mapM newFork [1..5]
    let philosophers = zipWith3 newPhilosopher philNames forks (tail forks ++ [head forks])
    putStrLn "Running the philosophers. Press enter to quit"
    futures <- Monad.mapM (async . runPhilosopher 1) philosophers
    exit <- race1 (waitAny futures) (Monad.void getLine)
    case exit of
        Left _  -> putStrLn "One philosopher done."
        Right _ -> putStrLn "Tired of waiting?"


race1 :: IO a -> IO b -> IO (Either a b)
race1 act1 act2 = do
    f1 <- async act1
    f2 <- async act2
    waitEither f1 f2

