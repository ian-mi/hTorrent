module BEncodeI where

import Control.Applicative
import Control.Lens
import Data.ByteString
import Data.Char
import Data.Map
import Text.Parsec as Par hiding ((<|>), many)

data BEncode = BString String | BInt Int | BList [BEncodeI] |
                BDict (Map String BEncodeI) deriving Show

data BEncodeI = BEncodeI {  _bencode :: BEncode,
                            _start :: Int,
                            _end :: Int } deriving Show

bString :: Prism' BEncode String
bString = prism' BString f
    where   f (BString s) = Just s
            f _ = Nothing

bInt :: Prism' BEncode Int
bInt = prism' BInt f
    where   f (BInt i) = Just i
            f _ = Nothing

bList :: Prism' BEncode [BEncodeI]
bList = prism' BList f
    where   f (BList bs) = Just bs
            f _ = Nothing

bDict :: Prism' BEncode (Map String BEncodeI)
bDict = prism' BDict f
    where   f (BDict m) = Just m
            f _ = Nothing

$(makeLenses ''BEncodeI)

charI :: Char -> Parsec ByteString Int Char
charI c = char c <* modifyState (+1)

digitI :: Parsec ByteString Int Char
digitI = digit <* modifyState (+1)

natI :: Parsec ByteString Int Int
natI =  f . fmap digitToInt <$> some digitI
    where   f :: [Int] -> Int
            f = Prelude.foldl ((+) . (*10)) 0

takeI :: Int -> Parsec ByteString Int String
takeI i = Par.count i anyChar <* modifyState (+i)

parseStringI :: Parsec ByteString Int String
parseStringI = natI <* charI ':' >>= takeI

parseIntI :: Parsec ByteString Int Int
parseIntI = charI 'i' *> natI <* charI 'e'

parseListI :: Parsec ByteString Int [BEncodeI]
parseListI = charI 'l' *> many parseBEncodeI <* charI 'e'

parseDictI :: Parsec ByteString Int (Map String BEncodeI)
parseDictI = fromList <$> (charI 'd' *> many parsePairI <* charI 'e')
    where parsePairI = (,) <$> parseStringI <*> parseBEncodeI

parseBEncodeI :: Parsec ByteString Int BEncodeI
parseBEncodeI = do  s <- getState
                    b <- m
                    e <- getState
                    return (BEncodeI b s e)
    where m =   BString <$> parseStringI <|>
                BInt <$> parseIntI <|>
                BList <$> parseListI <|>
                BDict <$> parseDictI

bLookupI :: String -> BEncodeI -> Maybe BEncodeI
bLookupI s b = b ^? bencode . bDict . at s . _Just

bLookup :: String -> Traversal' BEncode a -> BEncodeI -> Maybe a
bLookup s p b = bLookupI s b >>= (^? bencode . p)
