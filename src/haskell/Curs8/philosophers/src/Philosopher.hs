module Philosopher (Philosopher, newPhilosopher, runPhilosopher) where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Monad          as Monad
import           System.Random

import           Fork

data Philosopher = Philosopher
    { name  :: String
    , left  :: Fork
    , right :: Fork
    }

newPhilosopher :: String -> Fork -> Fork -> Philosopher
newPhilosopher = Philosopher

runPhilosopher :: Int -> Philosopher -> IO ()
runPhilosopher n (Philosopher name left right) = Monad.replicateM_ n $ do
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is hungry.")
    (f1, f2) <- atomically $ do
        { f1 <- takeFork left ; f2 <- takeFork right ; return (f1,f2) }
    putStrLn (name ++ " got forks " ++ show f1 ++ " and " ++ show f2 ++ " and is now eating.")
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is done eating. Going back to thinking.")
    atomically $ do { releaseFork left f2 ; releaseFork right f1 }

