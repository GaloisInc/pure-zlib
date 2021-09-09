module Codec.Compression.Zlib.Adler32(
         AdlerState
       , initialAdlerState
       , advanceAdler
       , finalizeAdler
       )
 where

import Data.Bits(shiftL, (.|.))
import Data.Word(Word8, Word32)

data AdlerState = AdlerState { adlerA :: !Word32, adlerB :: !Word32 }

initialAdlerState :: AdlerState
initialAdlerState = AdlerState 1 0

adlerAdd :: Word32 -> Word32 -> Word32
adlerAdd x y = (x + y) `mod` 65521
{-# INLINE adlerAdd #-}

advanceAdler :: AdlerState -> Word8 -> AdlerState
advanceAdler state b = AdlerState a' b'
 where
  a' = adlerAdd (adlerA state) (fromIntegral b)
  b' = adlerAdd (adlerB state) a'

finalizeAdler :: AdlerState -> Word32
finalizeAdler state = ((adlerB state) `shiftL` 16) .|.  adlerA state


