module Peer.State where

import HTorrentPrelude
import Data.Chunk
import Peer.Event

import Data.IntervalSet
import Network.Socket

data ConnectionState = ConnectionState {
    _interested :: TVar Bool,
    _choked :: TVar Bool }
$(makeLenses ''ConnectionState)

data PeerState = PeerState {
    _address :: SockAddr,
    _peerId :: ByteString,
    _localState :: ConnectionState,
    _remoteState :: ConnectionState,
    _pieces :: TVar IntSet,
    _requested :: TVar (IntMap IntervalSet),
    _peerPendingRequests :: TVar (Set Chunk),
    _cancelledRequests :: TQueue Chunk,
    _events :: TChan PeerEvent
}
$(makeLenses ''PeerState)

initConnectionState :: IO ConnectionState
initConnectionState = ConnectionState <$> newTVarIO False <*> newTVarIO True

initPeerState :: SockAddr -> ByteString -> IO PeerState
initPeerState a id = do
    s <- initConnectionState
    r <- initConnectionState
    ps <- newTVarIO mempty
    rq <- newTVarIO mempty
    rqs <- newTVarIO mempty
    es <- newBroadcastTChanIO
    cd <- newTQueueIO
    return PeerState {
        _address = a,
        _peerId = id,
        _localState = s,
        _remoteState = r,
        _pieces = ps,
        _requested = rq,
        _peerPendingRequests = rqs,
        _cancelledRequests = cd,
        _events = es }
