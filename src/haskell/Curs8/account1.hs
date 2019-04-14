module Curs8.Account1 where

import           Curs7.SpawnJoin

import           Control.Concurrent
import qualified Control.Monad      as Monad

data Account = Account
    { getAccount :: MVar Int
    , getName    :: String
    }

createAccount :: String -> Int -> IO Account
createAccount name amount = do
    acct <- newMVar amount
    return (Account acct name)

-- | Deposit into given account
deposit :: Account -> Int -> IO ()
deposit account amount = do
    x <- takeMVar acct
    putMVar acct (x + amount)
  where acct = getAccount account

-- | Withdraw from given account
withdraw :: Account -> Int -> IO Bool
withdraw account amount = do
    x <- takeMVar acct
    if x > amount   then do putMVar acct (x - amount)
                            return True
                    else do putMVar acct x
                            return False
  where acct = getAccount account

showBalance :: Account -> IO ()
showBalance (Account acct name) = do
    x <- readMVar acct
    putStrLn ("Account " ++ name ++ ": " ++ show x)

transfer :: Account -> Account -> Int -> IO Bool
transfer account1 account2 amount = do
    b <- withdraw account1 amount
    --threadDelay $ amount * 10^3
    Monad.when b (deposit account2 amount)
    return b

main :: IO ()
main = do
    janeAcc <- createAccount "Jane Doe" 1000
    johnAcc <- createAccount "John Smith" 1000
    tid1 <- spawn (Monad.void $ trans janeAcc johnAcc 300)
    tid2 <- spawn (Monad.void $ trans johnAcc janeAcc 500)
    forkIO (Monad.forever $
        do
        threadDelay (11^5)
        showBalance johnAcc
        )
    forkIO (Monad.forever $
        do
        threadDelay (10^5)
        showBalance janeAcc
        )
    join tid1
    join tid2
    showBalance janeAcc
    showBalance johnAcc
  where trans = transfer

transfer2 :: Account -> Account -> Int -> IO Bool
transfer2 account1 account2 amount = do
    y <- takeMVar acct2
    --threadDelay $ amount * 10^3
    x <- takeMVar acct1
    if x > amount   then do putMVar acct1 (x - amount)
                            putMVar acct2 (y + amount)
                            return True
                    else do putMVar acct1 x
                            putMVar acct2 y
                            return False
  where acct1 = getAccount account1
        acct2 = getAccount account2

