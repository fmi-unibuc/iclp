import           Control.Concurrent

printChars :: [Char] -> IO ()
printChars chars
  = do
    mapM_ print chars
    putChar '\n'

main :: IO ()
main
  = do
    forkIO (printChars ['a'..'z'])
    printChars ['A'..'Z']

