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
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 iNADDR_ANY)
    listen sock 2                              -- set a max of 2 queued connections
    putStrLn $ "Listening on " ++ show sock
    factor <- atomically $ newTVar 2
    forever $ do
        (asock, aaddr) <- accept sock
        putStrLn $ "Accepted connection from " ++ show aaddr
        handle <- socketToHandle asock ReadWriteMode
        forkFinally (talk handle factor) (\_ -> hClose handle)

talk :: Handle -> TVar Int -> IO ()
talk h factor = do
    hSetBuffering h LineBuffering
    c <- atomically newTChan
    race_ (server h c factor) (receive h c)

receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
    line <- hGetLine h
    atomically $ writeTChan c line

server :: Handle -> TChan String -> TVar Int -> IO ()
server h c factor = do
    f <- atomically $ readTVar factor
    hPutStrLn h $ "Current factor: " ++ show f
    loop f
  where
    loop f = join . atomically $ do
        f' <- readTVar factor
        if f /= f'
        then return (newFactor f')
        else do
            line <- readTChan c
            return (command f line)
    command f line = case line of
        'e':'n':'d':_ ->
            hPutStrLn h ("Thank you for using the "
                        ++ "Haskell doubling service.")
        '*':s -> do atomically $ writeTVar factor (read s :: Int)
                    loop f
        s -> do hPutStrLn h (show (f * (read s :: Int)))
                loop f
    newFactor f' = do   hPutStrLn h $ "New Factor: " ++ show f'
                        loop f'
