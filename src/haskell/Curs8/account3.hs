module Curs8.Account2 where

import           Curs7.SpawnJoin

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Monad          as Monad

data Account = Account
    { getAccount :: TVar Int
    , getName    :: String
    }

createAccount :: String -> Int -> IO Account
createAccount name amount = do
    acct <- atomically $ newTVar amount
    return (Account acct name)

-- | Deposit into given account
deposit :: Account -> Int -> STM ()
deposit account amount = do
    x <- readTVar acct
    writeTVar acct (x + amount)
  where acct = getAccount account

-- | Withdraw from given account
withdraw :: Account -> Int -> STM ()
withdraw account amount = do
    x <- readTVar acct
    Monad.when (x < amount) retry
    writeTVar acct (x - amount)
  where acct = getAccount account

showBalance :: Account -> IO ()
showBalance (Account acct name) = do
    x <- atomically $ readTVar acct
    putStrLn ("Account " ++ name ++ ": " ++ show x)

transfer :: Account -> Account -> Int -> STM ()
transfer account1 account2 amount = do
    withdraw account1 amount
    deposit account2 amount

transferFrom2 :: Account -> Account -> Account -> Int -> STM ()
transferFrom2 from1 from2 to amount = do
    withdraw from1 amount `orElse` withdraw from2 amount
    deposit to amount

main :: IO ()
main = do
    janeAcc <- createAccount "Jane Doe" 1000
    john1Acc <- createAccount "John Smith 1" 500
    john2Acc <- createAccount "John Smith 2" 500
    tid1 <- spawn (trans janeAcc john1Acc 1400)
    tid2 <- spawn (trans2 john1Acc john2Acc janeAcc 600)
    tid1 <- spawn (threadDelay (10*10^6) >> trans janeAcc john2Acc 100)
    forkIO (Monad.forever $
        do
        threadDelay (11^5)
        showBalance john1Acc
        showBalance john2Acc
        )
    forkIO (Monad.forever $
        do
        threadDelay (10^5)
        showBalance janeAcc
        )
    join tid1
    join tid2
    showBalance janeAcc
    showBalance john1Acc
    showBalance john2Acc
  where trans a1 a2 s = atomically $ transfer a1 a2 s
        trans2 a1 a2 a3 s = atomically $ transferFrom2 a1 a2 a3 s

