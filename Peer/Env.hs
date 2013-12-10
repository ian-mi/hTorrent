module Peer.Env where

import HTorrentPrelude
import Peer.Event
import Peer.Message

import qualified Data.IntSet as IS
import qualified Data.Set as S

data ConnectionState = ConnectionState {
    _interested :: TVar Bool,
    _choked :: TVar Bool }
$(makeLenses ''ConnectionState)

data PeerEnv = PeerEnv {
    _localState :: ConnectionState,
    _remoteState :: ConnectionState,
    _pieces :: TVar IntSet,
    _pendingRequests :: TVar (Set ChunkInd),
    _peerEvents :: TChan PeerEvent }
$(makeLenses ''PeerEnv)

initConnectionState :: IO ConnectionState
initConnectionState = ConnectionState <$> newTVarIO False <*> newTVarIO True

initPeerEnv :: IO PeerEnv
initPeerEnv = do
    s <- initConnectionState
    r <- initConnectionState
    ps <- newTVarIO IS.empty
    pending <- newTVarIO S.empty
    events <- newBroadcastTChanIO
    return PeerEnv {
        _localState = s,
        _remoteState = r,
        _pieces = ps,
        _pendingRequests = pending,
        _peerEvents = events }
