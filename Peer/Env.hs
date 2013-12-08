module Peer.Env where

import Peer.Message

import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Lens
import qualified Data.IntSet as IS
import qualified Data.Set as S

data ConnectionState = ConnectionState {
    _interested :: TVar Bool,
    _choked :: TVar Bool }
$(makeLenses ''ConnectionState)

data PeerEnv = PeerEnv {
    _localState :: ConnectionState,
    _peerState :: ConnectionState,
    _pieces :: TVar IS.IntSet,
    _pendingRequests :: TVar (S.Set ChunkInd) }
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
