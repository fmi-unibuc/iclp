module Semaphore.Binary (Semaphore, newSemaphore, acquire, release) where

import           Control.Concurrent

newtype Semaphore = Semaphore { getMVar :: MVar () }

newSemaphore :: IO Semaphore
newSemaphore = Semaphore <$> newMVar ()

acquire :: Semaphore -> IO ()
acquire = takeMVar . getMVar

release :: Semaphore -> IO ()
release = takeMVar . getMVar
