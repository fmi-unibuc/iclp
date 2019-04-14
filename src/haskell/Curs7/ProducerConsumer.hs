module ProducerConsumer (produce, consume) where
import           Channel
import           Control.Monad (forever, replicateM_)

produce :: Channel channel => channel a -> IO a -> IO ()
produce channel pAction = forever $ do
    msg <- pAction
    send channel msg

consume :: Channel channel => channel a -> (a -> IO ()) -> Int -> IO ()
consume channel cAction size = replicateM_ size $ do
    msg <- receive channel
    cAction msg
