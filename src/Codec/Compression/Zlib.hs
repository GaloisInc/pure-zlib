{-# LANGUAGE MultiWayIf #-}
module Codec.Compression.Zlib(
         DecompressionError(..)
       , decompress
       )
 where

import Codec.Compression.Zlib.Deflate
import Codec.Compression.Zlib.Monad
import Data.Bits
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Word

decompress :: ByteString -> Either DecompressionError ByteString
decompress ifile =
  case BS.uncons ifile of
    Nothing -> Left (HeaderError "Could not read CMF.")
    Just (cmf, rest) ->
     case BS.uncons rest of
       Nothing -> Left (HeaderError "Could not read FLG.")
       Just (flg, body) ->
         runDecompression cmf flg body

runDecompression :: Word8 -> Word8 -> ByteString ->
                    Either DecompressionError ByteString
runDecompression cmf flg body
   | both `mod` 31 /= 0 = Left (HeaderError ("Header checksum failed"))
   | cm        /= 8     = Left (HeaderError ("Bad method ("++show cm++")"))
   | cinfo     >  7     = Left (HeaderError "Window size too big.")
   | otherwise          = runDeflateM inflate body'
 where
  cm     = cmf .&. 0x0f
  cinfo  = cmf `shiftR` 4
  fdict  = testBit flg 5
--  flevel = flg `shiftR` 6
  --
  body' | fdict     = BS.drop 4 body
        | otherwise = body
  --
  both  :: Word16
  both   = (fromIntegral cmf `shiftL` 8) .|. fromIntegral flg
