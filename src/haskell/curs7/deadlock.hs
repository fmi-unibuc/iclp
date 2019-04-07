import           Control.Concurrent

main :: IO ()
main = do
    m <- newEmptyMVar
    takeMVar m
