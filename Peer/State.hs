module PeerState where

import PeerMessage
import Torrent

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.State.Class
import qualified Data.ByteString as BS
import qualified Data.IntSet as IS
import Data.IntMap
import qualified Data.Set as S

data PeerState = PeerState {    _choked :: TVar Bool,
                                _interested :: TVar Bool,
                                _hasPieces :: TVar IS.IntSet,
                                _pendingRequests :: TVar (S.Set ChunkInd),
                                _request :: TQueue ChunkInd,
                                _cancelled :: TQueue ChunkInd,
                                _torrentState :: TorrentState
                                }
$(makeLenses ''PeerState)
