import           Control.Monad          (replicateM_)
import           Data.IORef
import           Semaphore.Synchronized
import           SpawnJoin

work :: IORef a -> (a -> a) -> Synchronized -> IO ()
work ref f mutex = replicateM_ 5800000 $
        synchronized mutex (modifyIORef' ref f)

main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            mutex <- newSynchronized
            th1Id <- spawn (work ref (+ 1) mutex)
            th2Id <- spawn (work ref (+ (-1)) mutex)
            join th1Id
            join th2Id
            val <- readIORef ref
            print val
