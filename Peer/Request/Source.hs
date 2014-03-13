module Peer.Request.Source where

import HTorrentPrelude
import Data.Chunk
import Data.Interval
import Data.IntervalSet
import qualified Data.IntervalInverseMap as InvM
import Peer.Env
import Peer.State
import Torrent.Env
import Torrent.State.Availability
import Torrent.State.Downloading

import Data.Conduit.Combinators hiding (null)
import qualified Data.IntMap as IM
import Data.IntSet.Lens
import Data.Random

rqLen :: Int
rqLen = 2^14

requestSource :: Source (ReaderT PeerEnv IO) Chunk
requestSource = repeatM nextPiece $= awaitForever requestChunks

requestChunks :: Int -> Conduit Int (ReaderT PeerEnv IO) Chunk
requestChunks p = do
    m <- view (downloadingPieces . at p) <$> viewTVarIO (torrentEnv . downloading)
    case m of
        Nothing -> return ()
        Just st -> do
            r <- lift (waitUnchoked >> tryRequest p st)
            case r of
                Just i -> do
                    liftSTM (modifyTVar st (request i))
                    yield (Chunk p i)
                    requestChunks p
                Nothing -> return ()

waitUnchoked :: ReaderT PeerEnv IO ()
waitUnchoked = hoist atomically $ do
    c <- viewTVar (peerState . remoteState . choked)
    if c then lift retry else return ()

tryRequest :: Int -> TVar PieceState -> ReaderT PeerEnv IO (Maybe Interval)
tryRequest p st = do
    rq <- view (at p . from nonEmpty) <$> viewTVarIO (peerState . requested)
    nextRequest rq <$> lift (readTVarIO st)

nextPiece :: ReaderT PeerEnv IO Int
nextPiece = do
    ps <- waitInterested
    peerState . localState . interested !.= True
    rs <- magnify (torrentEnv . availability) (ReaderT (atomically . rarest ps))
    lift (sample (randomElement rs))

waitInterested :: ReaderT PeerEnv IO IntSet
waitInterested = do
    ps <- hoist atomically interestedPieces
    if null ps
        then do
            peerState . localState . interested !.= False
            hoist atomically waitInterestedPieces
        else return ps

waitInterestedPieces :: ReaderT PeerEnv STM IntSet
waitInterestedPieces = do
    ps <- interestedPieces
    if not (null ps) then return ps else lift retry

interestedPieces :: ReaderT PeerEnv STM IntSet
interestedPieces = do
    dP <- views downloadingPieces IM.keysSet <$> viewTVar (torrentEnv . downloading)
    ps <- viewTVar (peerState . pieces)
    rq <- IM.keysSet <$> viewTVar (peerState . requested)
    return (difference (intersection dP ps) rq)

nextRequest :: IntervalSet -> PieceState -> Maybe Interval
nextRequest rq st = truncateInterval rqLen <$> nextNeeded rq st

truncateInterval :: Int -> Interval -> Interval
truncateInterval len (Interval a b) = Interval a (min b (a + len - 1))
