module PhilosopherTMVar (Philosopher, newPhilosopher, runPhilosopher, finiteRunPhilosopher) where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Monad          as Monad
import           System.Random

import           ForkTMVar

data Philosopher = Philosopher
    { name  :: String
    , left  :: Fork
    , right :: Fork
    }

newPhilosopher :: String -> Fork -> Fork -> Philosopher
newPhilosopher = Philosopher

philosopherAction :: Philosopher -> IO ()
philosopherAction (Philosopher name left right) = do
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is hungry.")
    (lefty,righty) <- atomically
        $ (,) <$> takeFork left <*> takeFork right
    putStrLn (name ++ " got forks " ++ show lefty ++ " and "
                    ++ show righty ++ " and is now eating.")
    delay <- randomRIO (1,10)
    threadDelay (delay * 10^6)
    putStrLn (name ++ " is done eating. Going back to thinking.")
    atomically $ do { releaseFork left lefty ; releaseFork right righty }

runPhilosopher :: Philosopher -> IO ()
runPhilosopher = Monad.forever . philosopherAction

finiteRunPhilosopher :: Int -> Philosopher -> IO ()
finiteRunPhilosopher n p = do
    Monad.replicateM_ n (philosopherAction p)
    putStrLn (name p ++ " is leaving.")

