import           Control.Concurrent

main :: IO ()
main = do
    m <- newMVar 5
    n <- newMVar 6
    forkIO $ do a <- takeMVar n
                b <- takeMVar m
                putMVar m a
                putMVar n b
    b <- takeMVar m
    threadDelay 10
    a <- takeMVar n
    putMVar n b
    putMVar m a
