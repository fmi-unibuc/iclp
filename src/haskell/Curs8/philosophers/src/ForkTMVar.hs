module ForkTMVar (Fork, newFork, takeFork, releaseFork) where

import           Control.Concurrent.STM
import qualified Control.Monad          as Monad

newtype Fork = Fork { hasFork :: TMVar Int }

newFork :: Int -> IO Fork
newFork x = Fork <$> atomically (newTMVar x)

takeFork :: Fork -> STM Int
takeFork (Fork fork) = takeTMVar fork

releaseFork :: Fork -> Int -> STM ()
releaseFork (Fork fork) = putTMVar fork

