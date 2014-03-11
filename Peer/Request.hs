module Peer.Request (requestThread) where

import HTorrentPrelude
import Control.Concurrent.STM.Lens
import Data.Chunk
import Morphisms
import Peer.Env
import Peer.State
import Peer.Request.Source
import Torrent.Env

import Control.Concurrent.STM.State
import Control.Monad.STM.Class
import Data.Conduit.List
import Data.Conduit.TQueue
import Data.Interval
import Data.IntervalSet

numPipeline :: Int
numPipeline = 1

requestThread :: TQueue Chunk -> ReaderT PeerEnv IO ()
requestThread rs =
    requestSource $= iterM addRequest $= pipeline $$ sinkTQueue rs

addRequest :: Chunk -> ReaderT PeerEnv IO ()
addRequest c@(Chunk p i) = do
    peerState . requested !%= over (at p . from nonEmpty) (execState (addInterval i))
    peerState . pendingRequests !%= insertSet c

pipeline :: Conduit Chunk (ReaderT PeerEnv IO) Chunk
pipeline = forever (lift needRequests >>= isolate)

needRequests :: ReaderT PeerEnv IO Int
needRequests = hoist atomically $ do
    r <- olength <$> viewTVar (peerState . pendingRequests)
    if r < numPipeline then return (numPipeline - r) else lift retry
