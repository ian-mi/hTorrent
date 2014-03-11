module Piece where

import Files
import Files.MMap
import HTorrentPrelude
import Torrent.Env
import Torrent.Event
import Torrent.State.Downloading

import Crypto.Hash.SHA1
import Data.Array
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.IntervalInverseMap

completePiece :: Int -> ReaderT TorrentEnv IO ()
completePiece p = do
    m <- hoist atomically $ do
        r <- views downloadingPieces (lookup p) <$> viewTVar downloading
        case r of
            Nothing -> return Nothing
            Just st -> do
                s <- lift (readTVar st)
                if (null (s ^. requestedChunks . intervalInverseMap))
                    then do
                        downloading &%= (downloadingPieces %~ deleteMap p)
                        return (Just s)
                    else return Nothing
    case m of
        Nothing -> return ()
        Just (PieceState _ cs) -> do
            d <- lift (mappedChunksSource cs $$ sinkLbs)
            completed !%= insertMap p (toStrict d)
            numCompleted !%= succ
            torrentEvents !-< PieceCompleted p
