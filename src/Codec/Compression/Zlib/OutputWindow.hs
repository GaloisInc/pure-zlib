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

import           Data.ByteString.Builder(Builder, toLazyByteString, word8, lazyByteString)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Codec.Compression.Zlib.FingerTree(FingerTree, empty, (|>), dropTakeCombine, split, measure, toBuilder)
import           Data.Int(Int64)
import           Data.Semigroup as Sem
import           Data.Word(Word8)
import           Prelude()
import           Prelude.Compat

type WindowType = FingerTree S.ByteString

data OutputWindow = OutputWindow {
       owWindow    :: WindowType
     , owRecent    :: Builder
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
  (excessFT, window') = split excessAmount window
  excess              = toLazyByteString (toBuilder excessFT)

finalizeWindow :: OutputWindow -> L.ByteString
finalizeWindow ow = toLazyByteString (toBuilder (owWindow ow) <> owRecent ow)

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
  (prev, sme) = split dropAmt output
  chunkBase   = dropTakeCombine (dropAmt - measure prev) (fromIntegral len) sme
  chunkInf    = chunkBase `L.append` chunkInf
  chunk       = L.take len chunkInf