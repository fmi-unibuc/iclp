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

runPhilosopher :: Philosopher -> IO ()
runPhilosopher (Philosopher name left right) = Monad.forever $ do
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is hungry.")
    atomically $ do { takeFork left ; takeFork right }
    putStrLn (name ++ " got two forks and is now eating.")
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is done eating. Going back to thinking.")
    atomically $ do { releaseFork left ; releaseFork right }

