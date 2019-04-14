module Semaphore.Binary (Semaphore, newSemaphore, acquire, release) where

import           Control.Concurrent

newtype Semaphore = Semaphore { getSemaphore :: MVar () }

newSemaphore :: IO Semaphore
newSemaphore = Semaphore <$> newMVar ()

acquire :: Semaphore -> IO ()
acquire s = takeMVar (getSemaphore s)

release :: Semaphore -> IO ()
release s = putMVar (getSemaphore s) ()
