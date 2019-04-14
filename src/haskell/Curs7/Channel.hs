module Curs7.Channel (Channel, newChannel, send, receive) where

import           Control.Concurrent

class Channel channel where
    newChannel :: IO (channel a)
    send :: channel a -> a -> IO ()
    receive :: channel a -> IO a
