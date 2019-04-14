import           BlockingChannel
import           Channel
import           Control.Concurrent
import           Data.Char
import           ProducerConsumer
import           SpawnJoin

newBlockingChannel :: IO (BlockingChannel String)
newBlockingChannel = newChannel

main :: IO ()
main = do
    channel <- newBlockingChannel
    tid <- spawn
        (produce channel $ do x <- getLine
                              return (fmap toUpper x ++ "!!")
        )
    x <- newMVar ""
    consume channel (\str -> do v <- takeMVar x
                                putMVar x (v ++ str ++ "\n")
                    )
        10
    result <- takeMVar x
    putStrLn result
    join tid

