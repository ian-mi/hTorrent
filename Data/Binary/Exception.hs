module Data.Binary.Exception where

import ClassyPrelude
import Control.Monad.Exception.Synchronous
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS

data ParseErrorLazy = ParseErrorLazy LBS.ByteString ByteOffset String deriving Show

runGetExcept :: Get a -> LBS.ByteString -> Exceptional ParseErrorLazy a
runGetExcept get bs = case runGetOrFail get bs of
    Left (rem, off, msg) -> Exception (ParseErrorLazy rem off msg)
    Right (_, _, a) -> Success a
