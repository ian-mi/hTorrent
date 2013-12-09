module BEncode (BEncode(..),
                parseBEncode,
                bString,
                bInt,
                bList,
                bDict,
                bLookup) where


import HTorrentPrelude
import Control.Lens
import Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Map as M

data BEncode = BString ByteString | BInt Int | BList [BEncode] |
                BDict (Map ByteString BEncode) deriving Show

bString :: Prism' BEncode ByteString
bString = prism' BString f
    where   f (BString s) = Just s
            f _ = Nothing

bInt :: Prism' BEncode Int
bInt = prism' BInt f
    where   f (BInt i) = Just i
            f _ = Nothing

bList :: Prism' BEncode [BEncode]
bList = prism' BList f
    where   f (BList bs) = Just bs
            f _ = Nothing

bDict :: Prism' BEncode (Map ByteString BEncode)
bDict = prism' BDict f
    where   f (BDict m) = Just m
            f _ = Nothing

parseBEncode :: Parser BEncode
parseBEncode = BString <$> parseString <|> BInt <$> parseInt
                <|> BList <$> parseList <|> BDict <$> parseDict

parseString :: Parser ByteString
parseString = decimal <* char ':' >>= Atto.take

parseInt :: Parser Int
parseInt = char 'i' *> decimal <* char 'e'

parseList :: Parser [BEncode]
parseList = char 'l' *> many parseBEncode <* char 'e'

parseDict :: Parser (Map ByteString BEncode)
parseDict = M.fromList <$> (char 'd' *> many parsePair <* char 'e')
    where parsePair = (,) <$> parseString <*> parseBEncode

bLookup :: String -> Traversal' BEncode a -> BEncode -> Maybe a
bLookup s p b = b ^? bDict . at (fromString s) . _Just . p
