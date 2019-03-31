import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (replicateM_)
import           Data.IORef

work :: IORef a -> (a -> a) -> IO ()
work ref f = do replicateM_ 580000 (modifyIORef' ref f)
                putStrLn "done"

main :: IO ()
main = do   ref <- newIORef (0 :: Integer)
            forkIO (work ref (+ 1))
            forkIO (work ref (+ (-1)))
            threadDelay $ 1 * 10^6
            val <- readIORef ref
            print val
