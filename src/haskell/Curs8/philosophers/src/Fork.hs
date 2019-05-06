module Fork (Fork, newFork, takeFork, releaseFork) where

import           Control.Concurrent.STM
import qualified Control.Monad          as Monad

newtype Fork = Fork { hasFork :: TMVar Int }

newFork :: Int -> IO Fork
newFork i = Fork <$> atomically (newTMVar i)

takeFork :: Fork -> STM Int
takeFork (Fork fork) = takeTMVar fork

releaseFork :: Fork -> Int -> STM ()
releaseFork (Fork fork) i = putTMVar fork i

