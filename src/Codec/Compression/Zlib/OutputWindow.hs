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
import qualified Data.Semigroup.Compat as Sem
import           Data.Word(Word8)
import           Prelude()
import           Prelude.Compat

type WindowType = FingerTree Int S.ByteString

instance Sem.Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty  = 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

instance Measured Int S.ByteString where
  measure = S.length
  {-# INLINE measure #-}

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
  (excessFT, window') = split (>= excessAmount) window
  excess              = toLazyByteString (foldMap byteString excessFT)

finalizeWindow :: OutputWindow -> L.ByteString
finalizeWindow ow =
  toLazyByteString (foldMap byteString (owWindow ow) <> owRecent ow)

-- -----------------------------------------------------------------------------

addByte :: OutputWindow -> Word8 -> OutputWindow
addByte ow b = ow{ owRecent = owRecent ow <> word8 b }

addChunk :: OutputWindow -> L.ByteString -> OutputWindow
addChunk ow bs = ow{ owRecent = owRecent ow <> lazyByteString bs }

addOldChunk :: OutputWindow -> Int -> Int64 -> (OutputWindow, L.ByteString)
addOldChunk ow dist len = (OutputWindow output (lazyByteString chunk), chunk)
 where
  output      = L.foldlChunks (|>) (owWindow ow) (toLazyByteString (owRecent ow))
  dropAmt     = measure output - dist
  (prev, sme) = split (> dropAmt) output
  s :< rest   = viewl sme
  start       = S.take (fromIntegral len) (S.drop (dropAmt-measure prev) s)
  len'        = fromIntegral len - S.length start
  chunkBase   = getChunk rest len' (byteString start)
  chunkInf    = chunkBase `L.append` chunkInf
  chunk       = L.take len chunkInf

getChunk :: WindowType -> Int -> Builder -> L.ByteString
getChunk win len acc
  | len <= 0 = toLazyByteString acc
  | otherwise =
      case viewl win of
        EmptyL -> toLazyByteString acc
        cur :< rest ->
          let curlen = S.length cur
          in case compare (S.length cur) len of
               LT -> getChunk rest (len - curlen) (acc <> byteString cur)
               EQ -> toLazyByteString (acc <> byteString cur)
               GT -> let (mine, _notMine) = S.splitAt len cur
                     in toLazyByteString (acc <> byteString mine)
