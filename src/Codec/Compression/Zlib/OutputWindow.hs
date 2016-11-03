{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Codec.Compression.Zlib.OutputWindow(
         OutputWindow
       , emptyWindow
       , adjustWindow
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
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Monoid
import Data.Word

data OutputWindow = OutputWindow {
       owWindow    :: !L.ByteString
     , owRecent    :: !Builder
     }

emptyWindow :: OutputWindow
emptyWindow = OutputWindow L.empty mempty

adjustWindow :: OutputWindow -> (L.ByteString, OutputWindow)
adjustWindow ow
  -- One of the following cases must be true:
  -- Case #1: Between our old window and the new stuff we're adding, we still
  --          don't have enough to completely fill a window.
  | totalLen < 32768   = let ow' = OutputWindow (owWindow ow <> recent) mempty
                         in (L.empty, ow')
  -- Case #2: We've just added quite a lot of stuff; in fact, so much stuff,
  --          that the new window is entirely contained in that amount. So we
  --          should commit the old window and then *also* commit any excess
  --          data we have at the start of the new stuff.
  | recentLen >= 32768 = let ow' = OutputWindow recentWindow mempty
                         in (owWindow ow <> excessRecent, ow')
  -- Case #3: After adding the new stuff to our old window we're going to have
  --          too much data. Because we failed the test for #2, we know that the
  --          split point is back in our previous window. Beacuse we failed the
  --          test for #1, though, we know that there is excess data there. So
  --          we'll split off that bit and commit it, and then combine the
  --          remainders.
  | otherwise          = let ow' = OutputWindow (windowRemains <> recent) mempty
                         in (excessWindow, ow')
 where
  recent       = toLazyByteString (owRecent ow)
  curWindowLen = L.length (owWindow ow)
  recentLen    = L.length recent
  totalLen     = curWindowLen + recentLen
  -- these values are used if there's enough stuff in 'recent' that we can
  -- just toss our old window away
  (excessRecent, recentWindow) = L.splitAt (recentLen - 32768) recent
  -- these values are used if there's excess, but it's totally within the
  -- confines of the current window
  (excessWindow, windowRemains) = L.splitAt (totalLen - 32768) (owWindow ow)

finalizeWindow :: OutputWindow -> L.ByteString
finalizeWindow ow =
  toLazyByteString (lazyByteString (owWindow ow) <> (owRecent ow))

-- -----------------------------------------------------------------------------

addByte :: OutputWindow -> Word8 -> OutputWindow
addByte !ow !b = ow{ owRecent = owRecent ow <> word8 b }

addChunk :: OutputWindow -> L.ByteString -> OutputWindow
addChunk !ow !bs = ow{ owRecent = owRecent ow <> lazyByteString bs }

addOldChunk :: OutputWindow ->
               Int -> Int64 ->
               (OutputWindow, L.ByteString, L.ByteString)
addOldChunk !ow !dist !len =
  (ow' { owRecent = lazyByteString res }, output, res)
 where
  -- Generate the history by forcing a commit
  (output, ow') = adjustWindow ow
  window        = owWindow ow'
  windowLen     = L.length window
  offset        = windowLen - fromIntegral dist
  --
  base          = L.drop offset window
  baseLen       = L.length base
  res           = get len
  --
  get x | x == 0      = L.empty
        | x < baseLen = L.take x base
        | otherwise   = base <> get (x - baseLen)
