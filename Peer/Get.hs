module Peer.Get (getThread, GetEnv(..)) where

import Data.Chunk
import Files.MMap
import Peer.Env
import Peer.Event
import Peer.Get.Exception
import Peer.Get.Packet
import Peer.Message hiding (Piece)
import Peer.State
import Peer.Request.Complete
import Piece
import Morphisms
import Torrent.Env
import Torrent.Event
import Torrent.State.Availability
import Torrent.State.Downloading

import HTorrentPrelude
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Interval
import Data.IntervalSet
import Data.IntSet.Lens
import Data.Isometry
import Network.Socket

data GetEnv = GetEnv {
    _peerRequests :: TQueue Chunk,
    _peerCancelled :: TQueue Chunk,
    _peer :: PeerEnv
}
$(makeLenses ''GetEnv)

getThread :: GetEnv -> Socket -> IO ()
getThread env s = do
    r <- try (sourceSocket s $= messageConduit $$ handleMessages env)
    return ()

handleMessages :: GetEnv -> Consumer PeerMessage (ExceptT PeerGetE IO) ()
handleMessages env = CL.mapM_ (lift . flip runReaderT env . handlePeerMessage)

handlePeerMessage :: PeerMessage -> ReaderT GetEnv IO ()
handlePeerMessage ChokeMessage = do
    peer . peerState . remoteState . choked !.= True
    peer . peerState . events !-< ChokedRemote True
handlePeerMessage UnchokeMessage = do
    peer . peerState . remoteState . choked !.= False
    peer . peerState . events !-< ChokedRemote False
handlePeerMessage InterestedMessage = do
    peer . peerState . remoteState . interested !.= True
    peer . peerState . events !-< InterestedRemote True
handlePeerMessage UninterestedMessage = do
    peer . peerState . remoteState . interested !.= False
    peer . peerState . events !-< InterestedRemote False
handlePeerMessage (HaveMessage i) = hoist atomically $ do
    ps <- viewTVar (peer . peerState . pieces)
    when (notMember i ps) $ do
        peer . peerState . pieces &.= insertSet i ps
        magnify (peer . torrentEnv . availability) (incAvail i)
handlePeerMessage (BitfieldMessage b) = hoist atomically $ do
    ps <- viewTVar (peer . peerState . pieces)
    peer . peerState . pieces &.= b
    magnify (peer . torrentEnv . availability) $ do
        mapMOf_ members incAvail (difference b ps)
        mapMOf_ members decAvail (difference ps b)
handlePeerMessage (RequestMessage r) =
    view peerRequests >>= liftIO . atomically . flip writeTQueue r
handlePeerMessage (PieceMessage c@(Chunk p i) d) = do
    magnify (peer . torrentEnv) (addChunk c d)
    magnify peer (completeRequest c)
    peer . peerState . requested !%= over (at p . from nonEmpty) (execState (deleteInterval i))
    magnify (peer . torrentEnv) (completePiece p)
handlePeerMessage (CancelMessage c) = 
    view peerCancelled >>= liftIO . atomically . flip writeTQueue c

addChunk :: Chunk -> ByteString -> ReaderT TorrentEnv IO ()
addChunk c@(Chunk p i) d = do
    m <- views downloadingPieces (lookup p) <$> viewTVarIO downloading
    case m of
        Nothing -> return ()
        Just v -> do
            st <- lift (readTVarIO v)
            lift (yield d $$ mappedChunksSink (restrict i (st ^. mappedChunks)))
            lift (atomically (modifyTVar v (complete i)))
