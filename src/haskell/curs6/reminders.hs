import           Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do   s <- getLine
            if s == "exit" then return ()
            else do forkIO (setReminder (read s))
                    main

setReminder :: Int -> IO ()
setReminder t = do
    putStrLn $
        "Reminder set for " ++ show t ++ " seconds."
    threadDelay $ t * 10^6
    putStrLn $
        "Reminder for " ++ show t ++ " seconds is up! BING!\BEL"
