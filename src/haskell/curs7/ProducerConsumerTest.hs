import           BlockingChannel
import           Channel
import           Control.Concurrent (forkIO)
import           ProducerConsumer

newBlockingChannel :: IO (BlockingChannel String)
newBlockingChannel = newChannel

main :: IO ()
main = do
    channel <- newBlockingChannel
    forkIO (produce channel getLine)
    consume channel putStrLn 10
