module Peer.Request (requestThread) where

import HTorrentPrelude
import Control.Concurrent.STM.Lens
import Data.Chunk
import Morphisms
import Peer.Env
import Peer.State
import Peer.Request.Source
import Torrent.Env
import Torrent.State.Requests (insertRequest)

import Control.Concurrent.STM.State
import Control.Monad.STM.Class
import Data.Conduit.List
import Data.Conduit.TQueue
import Data.Interval
import Data.IntervalSet

numPipeline :: Int
numPipeline = 10

requestThread :: TQueue Chunk -> ReaderT PeerEnv IO ()
requestThread rs =
    requestSource $= iterM addRequest $= pipeline $$ sinkTQueue rs

addRequest :: Chunk -> ReaderT PeerEnv IO ()
addRequest c@(Chunk p i) = do
    lift (putStrLn ("Requesting " ++ fromString (show c)))
    peerState . requested !%= over (at p . from nonEmpty) (execState (addInterval i))
    peerState . peerPendingRequests !%= insertSet c
    id <- view (peerState . peerId)
    torrentEnv . pendingRequests !%= insertRequest c id

pipeline :: Conduit Chunk (ReaderT PeerEnv IO) Chunk
pipeline = forever (lift needRequests >>= isolate)

needRequests :: ReaderT PeerEnv IO Int
needRequests = hoist atomically $ do
    r <- olength <$> viewTVar (peerState . peerPendingRequests)
    if r < numPipeline then return (numPipeline - r) else lift retry
