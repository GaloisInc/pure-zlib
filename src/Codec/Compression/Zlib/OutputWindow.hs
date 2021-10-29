{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Compression.Zlib.OutputWindow (
  OutputWindow,
  emptyWindow,
  emitExcess,
  finalizeWindow,
  addByte,
  addChunk,
  addOldChunk,
) where

import Control.Monad (foldM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import qualified Data.Primitive as Prim
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Primitive.Mutable as MV
import GHC.ST (ST (..))
import GHC.Word (Word8 (..))

windowSize :: Int
windowSize = 128 * 1024

data OutputWindow s = OutputWindow
  { owWindow :: {-# UNPACK #-} !(MV.MVector s Word8)
  , owNext :: {-# UNPACK #-} !Int
  }

emptyWindow :: ST s (OutputWindow s)
emptyWindow = do
  window <- MV.new windowSize
  return (OutputWindow window 0)

excessChunkSize :: Int
excessChunkSize = 32768

emitExcess :: OutputWindow s -> ST s (Maybe (S.ByteString, OutputWindow s))
emitExcess OutputWindow{owWindow = window, owNext = initialOffset}
  | initialOffset < excessChunkSize * 2 = return Nothing
  | otherwise = do
    toEmit <- V.freeze $ MV.slice 0 excessChunkSize window
    let excessLength = initialOffset - excessChunkSize
    -- Need move as these can overlap!
    MV.move (MV.slice 0 excessLength window) (MV.slice excessChunkSize excessLength window)
    let ow' = OutputWindow window excessLength
    return (Just (SBS.fromShort $ toByteString toEmit, ow'))

finalizeWindow :: OutputWindow s -> ST s S.ByteString
finalizeWindow ow = do
  -- safe as we're doing it at the end
  res <- V.unsafeFreeze (MV.slice 0 (owNext ow) (owWindow ow))
  pure $ SBS.fromShort $ toByteString res

-- -----------------------------------------------------------------------------

addByte :: OutputWindow s -> Word8 -> ST s (OutputWindow s)
addByte !ow !b = do
  let offset = owNext ow
  MV.write (owWindow ow) offset b
  return ow{owNext = offset + 1}

addChunk :: OutputWindow s -> L.ByteString -> ST s (OutputWindow s)
addChunk !ow !bs = foldM copyChunk ow (L.toChunks bs)

copyChunk :: OutputWindow s -> S.ByteString -> ST s (OutputWindow s)
copyChunk ow sbstr = do
  -- safe as we're never going to look at this again
  ba <- V.unsafeThaw $ fromByteString $ SBS.toShort sbstr
  let offset = owNext ow
      len = MV.length ba
  MV.copy (MV.slice offset len (owWindow ow)) ba
  return ow{owNext = offset + len}

addOldChunk :: OutputWindow s -> Int -> Int -> ST s (OutputWindow s, S.ByteString)
addOldChunk (OutputWindow window next) dist len = do
  -- zlib can ask us to copy an "old" chunk that extends past our current offset.
  -- The intention is that we then start copying the "new" data we just copied into
  -- place. 'copyChunked' handles this for us.
  copyChunked (MV.slice next len window) (MV.slice (next - dist) len window) dist
  result <- V.freeze $ MV.slice next len window
  return (OutputWindow window (next + len), SBS.fromShort $ toByteString result)

{- | A copy function that copies the buffers sequentially in chunks no larger than
 the stated size. This allows us to handle the insane zlib behaviour.
-}
copyChunked :: MV.MVector s Word8 -> MV.MVector s Word8 -> Int -> ST s ()
copyChunked dest src chunkSize = go 0 (MV.length src)
 where
  go _ 0 = pure ()
  go copied toCopy = do
    let thisChunkSize = min toCopy chunkSize
    MV.copy (MV.slice copied thisChunkSize dest) (MV.slice copied thisChunkSize src)
    go (copied + thisChunkSize) (toCopy - thisChunkSize)

-- TODO: these are a bit questionable. Maybe we can just pass around Vector Word8 in the client code?
fromByteString :: SBS.ShortByteString -> V.Vector Word8
fromByteString (SBS ba) =
  let len = Prim.sizeofByteArray (Prim.ByteArray ba)
      sz = Prim.sizeOf (undefined :: Word8)
   in V.Vector 0 (len * sz) (Prim.ByteArray ba)

toByteString :: V.Vector Word8 -> SBS.ShortByteString
toByteString (V.Vector offset len ba) =
  let sz = Prim.sizeOf (undefined :: Word8)
      !(Prim.ByteArray ba') = Prim.cloneByteArray ba (offset * sz) (len * sz)
   in SBS ba'
