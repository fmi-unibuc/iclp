import           Control.Concurrent
import           Control.Monad      (replicateM_)
import           SpawnJoin

work :: MVar a -> (a -> a) -> IO ()
work ref f = replicateM_ 5800000 $ do
        v <- takeMVar ref
        putMVar ref (f v)

main :: IO ()
main = do   ref <- newMVar (0 :: Integer)
            th1Id <- spawn (work ref (+ 1))
            th2Id <- spawn (work ref (+ (-1)))
            join th1Id
            join th2Id
            val <- readMVar ref
            print val
