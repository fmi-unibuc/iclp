module Fork (Fork, newFork, takeFork, releaseFork) where

import           Control.Concurrent.STM
import qualified Control.Monad          as Monad

newtype Fork = Fork { hasFork :: TVar Bool }

newFork :: IO Fork
newFork = Fork <$> atomically (newTVar True)

takeFork :: Fork -> STM ()
takeFork (Fork fork) = do
    b <- readTVar fork
    Monad.when (not b) retry
    writeTVar fork False

releaseFork :: Fork -> STM ()
releaseFork (Fork fork) = writeTVar fork True


