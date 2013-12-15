module Peer.State where

import HTorrentPrelude

import Peer.Event
import Peer.Message
import Network.Socket

data ConnectionState = ConnectionState {
    _interested :: TVar Bool,
    _choked :: TVar Bool }
$(makeLenses ''ConnectionState)

data PeerState = PeerState {
    _address :: SockAddr,
    _localState :: ConnectionState,
    _remoteState :: ConnectionState,
    _pieces :: TVar IntSet,
    _pendingRequests :: TVar (Set ChunkInd),
    _events :: TChan PeerEvent
}
$(makeLenses ''PeerState)

initConnectionState :: IO ConnectionState
initConnectionState = ConnectionState <$> newTVarIO False <*> newTVarIO True

initPeerState :: SockAddr -> IO PeerState
initPeerState a = do
    s <- initConnectionState
    r <- initConnectionState
    ps <- newTVarIO mempty
    pending <- newTVarIO mempty
    es <- newBroadcastTChanIO
    return PeerState {
        _address = a,
        _localState = s,
        _remoteState = r,
        _pieces = ps,
        _pendingRequests = pending,
        _events = es }
