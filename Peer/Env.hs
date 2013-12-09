module Peer.Env where

import HTorrentPrelude
import Peer.Message

import qualified Data.IntSet as IS
import qualified Data.Set as S

data ConnectionState = ConnectionState {
    _interested :: TVar Bool,
    _choked :: TVar Bool }
$(makeLenses ''ConnectionState)

data PeerEnv = PeerEnv {
    _localState :: ConnectionState,
    _peerState :: ConnectionState,
    _pieces :: TVar IntSet,
    _pendingRequests :: TVar (Set ChunkInd) }
$(makeLenses ''PeerEnv)

initConnectionState :: IO ConnectionState
initConnectionState = ConnectionState <$> newTVarIO False <*> newTVarIO True

initPeerEnv :: IO PeerEnv
initPeerEnv = do
    s <- initConnectionState
    p <- initConnectionState
    ps <- newTVarIO IS.empty
    pending <- newTVarIO S.empty
    return PeerEnv {
        _localState = s,
        _peerState = p,
        _pieces = ps,
        _pendingRequests = pending }
