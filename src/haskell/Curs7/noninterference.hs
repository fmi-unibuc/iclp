import           Control.Monad (replicateM_)
import           Data.IORef
import           SpawnJoin

work :: IORef a -> (a -> a) -> IO ()
work ref f = do
    replicateM_ 5800000
        (atomicModifyIORef' ref (\x -> (f x, ())))
    putStrLn "done"

main :: IO ()
main = do
    ref <- newIORef (0 :: Integer)
    th1Id <- spawn (work ref (+ 1))
    th2Id <- spawn (work ref (+ (-1)))
    join th1Id
    join th2Id
    val <- readIORef ref
    print val
