module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.List          (isPrefixOf)
import           Network.Socket
import           System.IO

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 iNADDR_ANY)
    listen sock 2                              -- set a max of 2 queued connections
    putStrLn $ "Listening on " ++ show sock
    forever $ do
        (asock, aaddr) <- accept sock
        putStrLn $ "Accepted connection from " ++ show aaddr
        handle <- socketToHandle asock ReadWriteMode
        forkFinally (talk handle) (\_ -> hClose handle)

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    loop
  where
    loop = do
        line <- hGetLine h
        if "end" `isPrefixOf` line
        then hPutStrLn h ("Thank you for using the "
                        ++ "Haskell doubling service.")
        else do hPutStrLn h (show (2 * (read line :: Integer)))
                loop
