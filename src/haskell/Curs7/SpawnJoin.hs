module Curs7.SpawnJoin (TId, spawn, join) where
import           Control.Concurrent
import           Curs7.BlockingChannel
import           Curs7.Channel

newtype TId = TId (BlockingChannel ())

spawn :: IO () -> IO TId
spawn action = do   done <- newChannel
                    forkIO $ do action
                                send done ()
                    return $ TId done

join :: TId -> IO ()
join (TId done) = receive done
