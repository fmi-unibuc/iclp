import           BlockingChannel
import           Channel
import           Control.Concurrent (forkIO)
import           NonBlockingChannel
import           ProducerConsumer

prodConsChannel :: IO (BlockingChannel String)
prodConsChannel = newChannel

main :: IO ()
main = do
    channel <- prodConsChannel
    forkIO (produce channel (return "Thread 1"))
    forkIO (produce channel (return "Thread 2"))
    consume channel putStrLn 100
