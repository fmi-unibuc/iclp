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
withdraw :: Account -> Int -> STM Bool
withdraw account amount = do
    x <- readTVar acct
    if x >= amount  then do writeTVar acct (x - amount)
                            return True
                    else return False
  where acct = getAccount account

showBalance :: Account -> IO ()
showBalance (Account acct name) = do
    x <- atomically $ readTVar acct
    putStrLn ("Account " ++ name ++ ": " ++ show x)

transfer :: Account -> Account -> Int -> STM Bool
transfer account1 account2 amount = do
    b <- withdraw account1 amount
    Monad.when b (deposit account2 amount)
    return b

main :: IO ()
main = do
    janeAcc <- createAccount "Jane Doe" 1000
    johnAcc <- createAccount "John Smith" 1000
    tid1 <- spawn (trans janeAcc johnAcc 300)
    tid2 <- spawn (trans johnAcc janeAcc 500)
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
  where trans a1 a2 s = Monad.void $ atomically $ transfer a1 a2 s

