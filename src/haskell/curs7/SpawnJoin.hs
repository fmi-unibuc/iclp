module SpawnJoin (TId, spawn, join) where
import           BlockingChannel
import           Channel
import           Control.Concurrent

newtype TId = TId (BlockingChannel ())

spawn :: IO () -> IO TId
spawn action = do   done <- newChannel
                    forkIO $ do action
                                send done ()
                    return $ TId done

join :: TId -> IO ()
join (TId done) = receive done
