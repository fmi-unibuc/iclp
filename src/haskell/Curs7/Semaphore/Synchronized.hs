module Semaphore.Synchronized
    (Synchronized, newSynchronized, synchronized)
  where

import           Control.Concurrent

newtype Synchronized = Synchronized (MVar ())

newSynchronized :: IO Synchronized
newSynchronized = Synchronized <$> newMVar ()

synchronized :: Synchronized -> IO () -> IO ()
synchronized (Synchronized mvar) action
  = do
    takeMVar mvar
    action
    putMVar mvar ()
