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

import           Data.ByteString.Builder(Builder, toLazyByteString, word8,
                                         lazyByteString, byteString)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.FingerTree(FingerTree, Measured, ViewL(..),
                                 empty, (|>), split, measure, viewl)
import           Data.Foldable.Compat(foldMap)
import           Data.Int(Int64)
import           Data.Monoid.Compat((<>))
import           Data.Word(Word8)
import           Prelude()
import           Prelude.Compat

type WindowType = FingerTree Int S.ByteString

instance Monoid Int where
  mempty  = 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

instance Measured Int S.ByteString where
  measure = S.length
  {-# INLINE measure #-}

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
