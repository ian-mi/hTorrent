module Peer.Request.Complete where

import HTorrentPrelude
import Data.Chunk
import Data.Interval
import Peer.Env
import Peer.State
import Torrent.Env
import Torrent.State.Requests

completeRequest :: Chunk -> ReaderT PeerEnv IO ()
completeRequest c = do
    id <- view (peerState . peerId)
    rs <- torrentEnv . pendingRequests !%%= removeRequests c
    ps <- viewTVar (torrentEnv . peers)
    let f (c, id) = mapMOf_ (at id . _Just) (runReaderT (cancelRequest c)) ps
    lift (mapMOf_ (folded . filtered (not . (== id) . snd)) f rs)
    peerState . peerPendingRequests !%= deleteSet c

cancelRequest :: Chunk -> ReaderT PeerState IO ()
cancelRequest c = do
    lift (putStrLn ("Cancelling " ++ fromString (show c)))
    peerPendingRequests !%= deleteSet c
    cancelledRequests !<< c
