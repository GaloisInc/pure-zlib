{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Codec.Compression.Zlib.OutputWindow(
         OutputWindow
       , emptyWindow
       , emitExcess
       , finalizeWindow
       , addByte
       , addChunk
       , addOldChunk
       )
 where

import Control.Monad(foldM)
import qualified Data.ByteString      as S
import Data.ByteString.Short.Internal(ShortByteString(SBS), fromShort, toShort)
import qualified Data.ByteString.Lazy as L
import GHC.Exts
    ( Int#,
      Int(..),
      ByteArray#,
      MutableByteArray#,
      Word#,
      (+#),
      (-#),
      (<#),
      (>#),
      (<=#),
      (>=#),
      isTrue#,
      newPinnedByteArray#,
      shrinkMutableByteArray#,
      unsafeFreezeByteArray#,
      sizeofByteArray#,
      sizeofMutableByteArray#,
      writeWord8Array#,
      copyByteArray#,
      copyMutableByteArray# )
import GHC.Int ( Int64(..) )
import GHC.ST ( ST(..) )
import GHC.Word ( Word8(..) )

windowSize :: Int
windowSize = 128 * 1024

data OutputWindow s = OutputWindow {
       owWindow    :: MutableByteArray# s 
     , owNext      :: Int#
     }

emptyWindow :: ST s (OutputWindow s)
emptyWindow =
  do MBA window <- newBuffer "emptyWindow" unliftedSize
     return (OutputWindow window 0#) 
 where
  !(I# unliftedSize) = windowSize

emitExcess :: OutputWindow s -> ST s (Maybe (S.ByteString, OutputWindow s))
emitExcess OutputWindow{ owWindow = window, owNext = initialOffset }
 | isTrue# (initialOffset <# 65536#) = return Nothing 
 | otherwise =
    do BA ba <- freeze window 0# 32768#
       () <- copy "emitExcess" window 32768# window 0# (initialOffset -# 32768#)
       let ow' = OutputWindow window (initialOffset -# 32768#)
           bstr = toByteString ba
       return (Just (bstr, ow'))

finalizeWindow :: OutputWindow s -> ST s S.ByteString
finalizeWindow ow = ST $ \ s0 ->
  case shrinkMutableByteArray# (owWindow ow) (owNext ow) s0 of
    s1 ->
      case unsafeFreezeByteArray# (owWindow ow) s1 of
        (# s2, resultBA #) ->
          (# s2, fromShort (SBS resultBA) #)

-- -----------------------------------------------------------------------------

addByte :: OutputWindow s -> Word8 -> ST s (OutputWindow s)
addByte !ow (W8# b) = do
  let offset = owNext ow
  () <- writeByte (owWindow ow) offset b
  return ow{ owNext = offset +# 1# }

addChunk :: OutputWindow s -> L.ByteString -> ST s (OutputWindow s)
addChunk !ow !bs = foldM copyChunk ow (L.toChunks bs)

copyChunk :: OutputWindow s -> S.ByteString -> ST s (OutputWindow s)
copyChunk ow sbstr = do
  let !(SBS ba) = toShort sbstr
      chunkSize = sizeofByteArray# ba
      offset = owNext ow
  () <- copyFrozen "copyChunk" ba 0# (owWindow ow) offset chunkSize
  return ow{ owNext = offset +# chunkSize }

addOldChunk :: OutputWindow s -> Int -> Int64 -> ST s (OutputWindow s, S.ByteString)
addOldChunk (OutputWindow window next) (I# dist) (I64# len) = do
  () <- copy "addOldChunk/copyToWindow" window (next -# dist) window next len
  BA resultBA <- freeze window (next -# dist) len
  return (OutputWindow window (next +# len), toByteString resultBA)

-- -----------------------------------------------------------------------------
-- Bounds checked (or slightly lifted) versions of various primitives

data BA = BA ByteArray#
data MBA s = MBA (MutableByteArray# s)

toByteString :: ByteArray# -> S.ByteString 
toByteString ba = fromShort (SBS ba)

newBuffer :: String -> Int# -> ST s (MBA s)
newBuffer src size = ST $ \ s0 ->
  case size <=# 0# of
    0# -> case newPinnedByteArray# size s0 of
            (# s1, res #) -> (# s1, MBA res #)
    _ -> error ("newBuffer/" ++ src ++ ": Got a buffer size <= 0")

copy :: String -> MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> ST s ()
copy src srcBuffer srcOff destBuffer destOff amt = 
  let srcBufferSize = sizeofMutableByteArray# srcBuffer
      destBufferSize = sizeofMutableByteArray# destBuffer
  in if | isTrue# (srcBufferSize <# (srcOff +# amt)) -> error ("copy/" ++ src ++ ": source buffer isn't big enough to handle offset + amt (" ++ show (I# srcBufferSize) ++ " < " ++ "(" ++ show (I# srcOff) ++ " + " ++ show (I# amt) ++ "))")
        | isTrue# (destBufferSize <# (destOff +# amt))  -> error ("copy/" ++ src ++ ": destination buffer isn't big enough to handle offset + amt")
        | isTrue# (srcOff <# 0#) -> error ("copy/" ++ src ++ ": negative source offset")
        | isTrue# (destOff <# 0#) -> error ("copy/" ++ src ++ ": negative destination offset")
        | isTrue# (amt <# 0#) -> error ("copy/" ++ src ++ ": negative amount to copy")
        | isTrue# ((srcOff +# amt) ># destOff) && isTrue# (srcOff <# destOff)-> do
           -- this is a moderately bonkers case that zlib allows. what is supposed to happen,
           -- I guess, is that bytes are written one by one so that by the time we catch up
           -- to the beginning, we've written the bytes we'll then copy. to deal with this
           -- we're going to break up the case so that first we copy the part we have, then
           -- we try again.
           let firstAmt = destOff -# srcOff
           copy (src ++ "H") srcBuffer srcOff destBuffer destOff firstAmt
           copy (src ++ "'") srcBuffer srcOff destBuffer (destOff +# firstAmt) (amt -# firstAmt)
        | otherwise -> ST $ \ s0 ->
            case copyMutableByteArray# srcBuffer srcOff destBuffer destOff amt s0 of
              s1 -> (# s1, () #)

copyFrozen :: String -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> ST s ()
copyFrozen src srcBuffer srcOff destBuffer destOff amt = ST $ \ s ->
  let srcBufferSize = sizeofByteArray# srcBuffer
      destBufferSize = sizeofMutableByteArray# destBuffer
  in if | isTrue# (srcBufferSize <# (srcOff +# amt)) -> error ("copyFrozen/" ++ src ++ ": source buffer isn't big enough to handle offset + amt (" ++ show (I# srcBufferSize) ++ " < " ++ "(" ++ show (I# srcOff) ++ " + " ++ show (I# amt) ++ "))")
        | isTrue# (destBufferSize <# (destOff +# amt))  -> error ("copyFrozen/" ++ src ++ ": destination buffer isn't big enough to handle offset + amt")
        | isTrue# (srcOff <# 0#) -> error ("copyFrozen/" ++ src ++ ": negative source offset")
        | isTrue# (destOff <# 0#) -> error ("copyFrozen/" ++ src ++ ": negative source offset")
        | otherwise ->
            case copyByteArray# srcBuffer srcOff destBuffer destOff amt s of
              s1 -> (# s1, () #)

freeze :: MutableByteArray# s -> Int# -> Int# -> ST s BA
freeze buffer off len
  | isTrue# (len <=# 0#) = error "Freeze passed a buffer of 0 or negative size"
  | isTrue# (sizeofMutableByteArray# buffer <=# (off +# len)) = error "Freeze passed an offset/length greater than the buffer size"
  | otherwise = ST $ \ s0 ->
      case newPinnedByteArray# len s0 of
        (# s1, mba #) ->
          case copyMutableByteArray# buffer off mba 0# len s1 of
            s2 ->
              case unsafeFreezeByteArray# mba s2 of
                (# s3, ba #) -> (# s3, BA ba #)

writeByte :: MutableByteArray# s -> Int# -> Word# -> ST s ()
writeByte buffer off b
  | isTrue# (off <# 0#) = error "writeByte to negative offset"
  | isTrue# (off >=# sizeofMutableByteArray# buffer) = error "writeByte after end of array"
  | otherwise = ST $ \ s ->
      case writeWord8Array# buffer off b s of 
        s' -> (# s', () #)
