module TMVar (TMVar, newEmptyTMVar, newTMVar, takeTMVar, putTMVar) where

import           Control.Concurrent.STM (TVar)

data TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

newTMVar :: a -> STM (TMVar a)
newTMVar x = TMVar <$> newTVar (Just x)

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
    mx <- readTVar t
    case mx of
        Nothing -> retry
        Just x -> do
            writeTVar t Nothing
            return x

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) x = do
    mx <- readTVar t
    case mx of
        Nothing -> writeTVar t (Just x)
        Just x  -> retry


takeBothTMVar :: TMVar a -> TMVar b -> STM (a,b)
takeBothTMVar tmva tmvb = (,) <$> takeTMVar tmva <*> takeTMVar tmvb


