module BlockingChannel (BlockingChannel) where

import           Channel
import           Control.Concurrent

newtype BlockingChannel a = BlockingChannel { getChannel :: MVar a }

instance Channel BlockingChannel where
    newChannel = BlockingChannel <$> newEmptyMVar
    send bChan x = putMVar (getChannel bChan) x
    receive bChan = takeMVar (getChannel bChan)
