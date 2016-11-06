{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

{-
Old timings:
------------
./.cabal-sandbox/bin/deflate all.z  5.88s user 0.14s system 98% cpu 6.088 total
./.cabal-sandbox/bin/deflate all.z  6.22s user 0.14s system 99% cpu 6.389 total
./.cabal-sandbox/bin/deflate all.z  5.96s user 0.13s system 99% cpu 6.124 total
./.cabal-sandbox/bin/deflate all.z  6.03s user 0.13s system 99% cpu 6.190 total
./.cabal-sandbox/bin/deflate all.z  6.00s user 0.13s system 99% cpu 6.160 total
./.cabal-sandbox/bin/deflate all.z  6.33s user 0.15s system 99% cpu 6.510 total
-}

import Data.ByteString.Builder
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.FingerTree
import Data.Int
import Data.Monoid
import Data.Word

type WindowType = FingerTree Int S.ByteString

instance Monoid Int where
  mempty  = 0
  mappend = (+)

instance Measured Int S.ByteString where
  measure = S.length

data OutputWindow = OutputWindow {
       owWindow    :: !WindowType
     , owRecent    :: !Builder
     }

emptyWindow :: OutputWindow
emptyWindow = OutputWindow empty mempty

emitExcess :: OutputWindow -> Maybe (L.ByteString, OutputWindow)
emitExcess ow
  | totalMeasure < 65536 = Nothing
  | otherwise            = Just (excess, ow{ owWindow = window' })
 where
  window              = owWindow ow
  totalMeasure        = measure window
  excessAmount        = totalMeasure - 32768
  (excessFT, window') = split (>= excessAmount) window
  excess              = toLazyByteString (foldMap byteString excessFT)

finalizeWindow :: OutputWindow -> L.ByteString
finalizeWindow ow =
  toLazyByteString (foldMap byteString (owWindow ow) <> owRecent ow)

-- -----------------------------------------------------------------------------

addByte :: OutputWindow -> Word8 -> OutputWindow
addByte !ow !b = ow{ owRecent = owRecent ow <> word8 b }

addChunk :: OutputWindow -> L.ByteString -> OutputWindow
addChunk !ow !bs = ow{ owRecent = owRecent ow <> lazyByteString bs }

addOldChunk :: OutputWindow -> Int -> Int64 -> (OutputWindow, L.ByteString)
addOldChunk !ow !dist !len = (OutputWindow output (lazyByteString chunk), chunk)
 where
  output      = L.foldlChunks (|>) (owWindow ow) (toLazyByteString (owRecent ow))
  dropAmt     = measure output - dist
  (prev, sme) = split (> dropAmt) output
  s :< rest   = viewl sme
  start       = S.take (fromIntegral len) (S.drop (dropAmt-measure prev) s)
  len'        = fromIntegral len - S.length start
  (m, rest')  = split (> len') rest
  middle      = L.toStrict (toLazyByteString (foldMap byteString m))
  end         = case viewl rest' of
                  EmptyL -> S.empty
                  bs2 :< _ -> S.take (len' - measure m) bs2
  chunkInf    = L.fromChunks [start, middle, end] `L.append` chunk
  chunk       = L.take len chunkInf
