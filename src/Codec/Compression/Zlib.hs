{-# LANGUAGE MultiWayIf #-}
module Codec.Compression.Zlib(
         DecompressionError(..)
       , ZlibDecoder(NeedMore, Chunk, Done, DecompError)
       , decompress
       , decompressIncremental
       )
 where

import           Codec.Compression.Zlib.Deflate(inflate)
import           Codec.Compression.Zlib.Monad(ZlibDecoder(..), DeflateM,
                                              DecompressionError(..),
                                              runDeflateM, raise, nextByte)
import           Control.Monad(unless, when, replicateM_)
import           Data.Bits((.|.), (.&.), shiftL, shiftR, testBit)
import           Data.ByteString.Builder(Builder, byteString,toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Word(Word16)
import GHC.ST(ST, runST)
import           Prelude()
import           Prelude.Compat

decompressIncremental :: ST s (ZlibDecoder s)
decompressIncremental = runDeflateM inflateWithHeaders

decompress :: L.ByteString -> Either DecompressionError L.ByteString
decompress ifile = runST $ do
  base <- decompressIncremental
  run base (L.toChunks ifile) mempty
 where
  run :: ZlibDecoder s -> [S.ByteString] -> Builder -> ST s (Either DecompressionError L.ByteString)
  run (NeedMore _) [] _ =
    return (Left (DecompressionError "Ran out of data mid-decompression 2."))
  run (NeedMore f) (first:rest) acc =
    do nextState <- f first
       run nextState rest acc
  run (Chunk c m) ls acc =
    do nextState <- m
       run nextState ls (acc <> byteString c)
  run Done        [] acc =
    return (Right (toLazyByteString acc))
  run Done        (_:_) _ =
    return (Left (DecompressionError "Finished with data remaining."))
  run (DecompError e) _ _ =
    return (Left e)

inflateWithHeaders :: DeflateM s ()
inflateWithHeaders =
  do cmf <- nextByte
     flg <- nextByte
     let both   = fromIntegral cmf `shiftL` 8 .|. fromIntegral flg
         cm     = cmf .&. 0x0f
         cinfo  = cmf `shiftR` 4
         fdict  = testBit flg 5
--       flevel = flg `shiftR` 6
     unless ((both :: Word16) `mod` 31 == 0) $
       raise (HeaderError "Header checksum failed")
     unless (cm == 8) $
       raise (HeaderError ("Bad compression method: " ++ show cm))
     unless (cinfo <= 7) $
       raise (HeaderError ("Window size too big: " ++ show cinfo))
     when fdict $ replicateM_ 4 nextByte -- just skip them for now (FIXME)
     inflate
