module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List                (isPrefixOf)
import           Network.Socket
import           System.IO

main :: IO ()
main = withSocketsDo $ do
    factor <- atomically $ newTVar 2
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 iNADDR_ANY)
    listen sock 2                              -- set a max of 2 queued connections
    putStrLn $ "Listening on " ++ show sock
    forever $ do
        (asock, aaddr) <- accept sock
        putStrLn $ "Accepted connection from " ++ show aaddr
        handle <- socketToHandle asock ReadWriteMode
        -- forkIO (talk handle)
        forkFinally (talk handle factor) (\_ -> hClose handle)

talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
    hSetBuffering h LineBuffering
    f <- atomically $ readTVar factor
    loop f
  where
    loop f = do
        hPutStrLn h $ "Multiplication using " ++ show f
        result <- race (checkFactor f)(multiply f)
        case result of
            Left f' -> loop f'
            Right (Just f') -> loop f'
            Right Nothing ->
                    hPutStrLn h ("Thank you for using the "
                            ++ "Haskell doubling service.")
    checkFactor f = atomically $ do
        f' <- readTVar factor
        when (f == f') retry
        return f'
    multiply f = do
            line <- hGetLine h
            case line of
                's':'e':'t':' ':s -> do
                    let f = read s :: Integer
                    atomically $ writeTVar factor f
                    return (Just f)
                'e':'n':'d':_  -> return Nothing
                _ -> do hPutStrLn h (show (f * (read line :: Integer)))
                        multiply f
