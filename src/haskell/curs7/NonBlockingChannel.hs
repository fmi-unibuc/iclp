module NonBlockingChannel (NonBlockingChannel) where

import           Channel
import           Control.Concurrent

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)

data NonBlockingChannel a =
    NonBlockingChannel
    { readEnd  :: MVar (Stream a)
    , writeEnd :: MVar (Stream a) -- always empty
    }

instance Channel NonBlockingChannel where
    newChannel = do
        stream <- newEmptyMVar
        NonBlockingChannel <$> newMVar stream <*> newMVar stream
    send channel a = do
        lastCell <- takeMVar (writeEnd channel)
        lastCell' <- newEmptyMVar
        putMVar lastCell (Item a lastCell')
        putMVar (writeEnd channel) lastCell'
    receive channel = do
        firstCell <- takeMVar (readEnd channel)
        (Item a firstCell') <- takeMVar firstCell
        putMVar (readEnd channel) firstCell'
        return a

