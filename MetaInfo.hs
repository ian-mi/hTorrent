module MetaInfo where

import BEncodeI

import HTorrentPrelude
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import Network.URI
import Text.Parsec hiding ((<|>))

data MetaInfo = MetaInfo {_announce :: URI, _info :: Info} deriving Show
data Info = Info {  _name :: String,
                    _piece_length :: Int,
                    _pieces :: ByteString,
                    _file :: Either Int [File],
                    _hash :: ByteString } deriving Show

data File = File {_fileLength :: Int, _path :: String} deriving Show

$(makeLenses ''MetaInfo)
$(makeLenses ''Info)
$(makeLenses ''File)

parseFile :: BEncodeI -> Maybe File
parseFile b =    File <$>
            bLookup "length" bInt b <*>
            bLookup "path" bString b

parseInfo :: BEncodeI -> ReaderT ByteString Maybe Info
parseInfo b = Info <$> n <*> p <*> ps <*> (Left <$> l <|> Right <$> fs) <*> getHash b
    where   n = lift (bLookup "name" bString b)
            p = lift (bLookup "piece length" bInt b)
            ps = lift (fromString <$> bLookup "pieces" bString b)
            l = lift (bLookup "length" bInt b)
            fs = lift (bLookup "files" bList b >>= mapM parseFile)

getHash :: BEncodeI -> ReaderT ByteString Maybe ByteString
getHash b = reader (SHA1.hash . BS.take (e - s) . BS.drop s)
    where   s = fromIntegral (_start b)
            e = fromIntegral (_end b)

parseMetaInfo :: BEncodeI -> ReaderT ByteString Maybe MetaInfo
parseMetaInfo b = MetaInfo <$> a <*> i
    where   a = lift (bLookup "announce" bString b >>= parseURI)
            i = lift (bLookupI "info" b) >>= parseInfo

readTorrent :: String -> IO (Maybe MetaInfo)
readTorrent s = readMetaInfo s <$> BS.readFile s

readMetaInfo :: String -> ByteString -> Maybe MetaInfo
readMetaInfo s bs = runReaderT (lift b >>= parseMetaInfo) bs
    where b = runParser parseBEncodeI 0 s bs ^? _Right
