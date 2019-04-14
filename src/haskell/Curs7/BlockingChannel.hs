module Curs7.BlockingChannel (BlockingChannel) where

import           Control.Concurrent
import           Curs7.Channel

newtype BlockingChannel a = BlockingChannel { getChannel :: MVar a }

instance Channel BlockingChannel where
    newChannel = BlockingChannel <$> newEmptyMVar
    send bChan x = putMVar (getChannel bChan) x
    receive bChan = takeMVar (getChannel bChan)
