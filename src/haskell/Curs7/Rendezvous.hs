module Rendezvous
  (Rendezvous, newRendezvous, leftShake, rightShake) where

import           Channel

data Rendezvous a b =
    Rendezvous { left :: Channel a, right :: Channel b }

newRendezvous :: IO (Rendezvous a b)
newRendezvous = do
    lChan <- newChannel
    rChan <- newChannel
    return $ Rendezvous lChan rChan

exchange :: Channel a -> Channel b -> a -> IO b
exchange channelA channelB a = do   send channelA a
                                    receive channelB

leftShake :: Rendezvous a b -> a -> IO b
leftShake rv = exchange (left rv) (right rv)

rightShake :: Rendezvous a b -> b -> IO a
rightShake rv = exchange (right rv) (left rv)
