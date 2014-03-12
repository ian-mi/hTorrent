module Peer.Request.Source where

import Data.Chunk
import Data.Interval
import Data.IntervalSet
import qualified Data.IntervalInverseMap as InvM
import Peer.Env
import Peer.State
import Torrent.Env
import Torrent.State.Downloading

import HTorrentPrelude

rqLen :: Int
rqLen = 2^14

requestSource :: Source (ReaderT PeerEnv IO) Chunk
requestSource = forever (lift nextPiece >>= uncurry requestPiece)

requestPiece :: Int -> TVar PieceState -> Source (ReaderT PeerEnv IO) Chunk
requestPiece p st = do
    lift waitUnchoked
    r <- lift (tryRequest p st)
    case r of
        Just i -> do
            liftSTM (modifyTVar st (request i))
            yield (Chunk p i)
            requestPiece p st
        Nothing -> return ()

waitUnchoked :: ReaderT PeerEnv IO ()
waitUnchoked = hoist atomically $ do
    c <- viewTVar (peerState . remoteState . choked)
    if c then lift retry else return ()

tryRequest :: Int -> TVar PieceState -> ReaderT PeerEnv IO (Maybe Interval)
tryRequest p st = do
    rq <- viewTVarIO (peerState . requested)
    nextRequest (rq ^. at p . from nonEmpty) <$> lift (readTVarIO st)

nextPiece :: ReaderT PeerEnv IO (Int, TVar PieceState)
nextPiece = do
    p <- hoist atomically tryNextPiece
    maybe waitInterested return p <* peerState . localState . interested !.= True

waitInterested :: ReaderT PeerEnv IO (Int, TVar PieceState)
waitInterested = do
    peerState . localState . interested !.= False
    hoist atomically waitNextPiece

waitNextPiece :: ReaderT PeerEnv STM (Int, TVar PieceState)
waitNextPiece = tryNextPiece >>= maybe (lift retry) return

tryNextPiece :: ReaderT PeerEnv STM (Maybe (Int, TVar PieceState))
tryNextPiece = do
    d <- viewTVar (torrentEnv . downloading)
    ps <- viewTVar (peerState . pieces)
    rd <- viewTVar (peerState . requested)
    lift (nextInterestedPiece d ps rd)

nextInterestedPiece :: Downloading -> IntSet -> IntMap IntervalSet ->
    STM (Maybe (Int, TVar PieceState))
nextInterestedPiece d ps rd = runMaybeT $ do
    (p, _) <- MaybeT (d ^@!? interestedPieces ps rd)
    MaybeT (return ((p,) <$> lookup p (d ^. downloadingPieces)))

interestedPieces :: IntSet -> IntMap IntervalSet ->
    IndexedMonadicFold Int STM Downloading PieceState
interestedPieces ps rd =
    downloadingPieces . ifolded . ifiltered f . act readTVar . ifiltered g
    where   f p _ = member p ps
            g p (PieceState rq _)
                | Just s <- lookup p rd = not (InvM.containedBy rq s)
                | otherwise = True

nextRequest :: IntervalSet -> PieceState -> Maybe Interval
nextRequest rq st = truncateInterval rqLen <$> nextNeeded rq st

truncateInterval :: Int -> Interval -> Interval
truncateInterval len (Interval a b) = Interval a (min b (a + len - 1))
