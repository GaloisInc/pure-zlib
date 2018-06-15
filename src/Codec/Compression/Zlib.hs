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
import           Data.ByteString.Builder(lazyByteString,toLazyByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Word(Word16)
import           Prelude()
import           Prelude.Compat

decompressIncremental :: ZlibDecoder
decompressIncremental = runDeflateM inflateWithHeaders

decompress :: L.ByteString -> Either DecompressionError L.ByteString
decompress ifile = run decompressIncremental (L.toChunks ifile) mempty
 where
  run (NeedMore _) [] _ =
    Left (DecompressionError "Ran out of data mid-decompression 2.")
  run (NeedMore f) (first:rest) acc =
    run (f first) rest acc
  run (Chunk c m) ls acc =
    run m ls (acc <> lazyByteString c)
  run Done        [] acc =
    Right (toLazyByteString acc)
  run Done        (_:_) _ =
    Left (DecompressionError "Finished with data remaining.")
  run (DecompError e) _ _ =
    Left e

inflateWithHeaders :: DeflateM ()
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
