import           Control.Concurrent

printChars :: [Char] -> IO ()
printChars chars
  = do
    mapM_ putChar chars
    putChar '\n'

main :: IO ()
main
  = do
    forkIO (printChars ['a'..'z'])
    printChars ['A'..'Z']

