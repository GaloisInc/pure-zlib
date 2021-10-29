{-# LANGUAGE MagicHash #-}
module Codec.Compression.Zlib.Adler32(
         AdlerState
       , initialAdlerState
       , advanceAdler
       , advanceAdlerBlock
       , finalizeAdler
       )
 where

import Data.Bits(shiftL, (.|.))
import qualified Data.ByteString as S
import GHC.Exts ( Word#, plusWord#, remWord# )
import GHC.Word ( Word8(..), Word32(..) )

data AdlerState = AdlerState { _adlerA :: Word#, _adlerB :: Word# }

initialAdlerState :: AdlerState
initialAdlerState = AdlerState 1## 0##

advanceAdler :: AdlerState -> Word8 -> AdlerState
advanceAdler (AdlerState a b) (W8# v) = AdlerState a' b'
 where
  a' = (a `plusWord#` v) `remWord#` 65521##
  b' = (b `plusWord#` a') `remWord#` 65521##
{-# INLINE advanceAdler #-}

advanceNoMod :: AdlerState -> Word8 -> AdlerState
advanceNoMod (AdlerState a b) (W8# v) = AdlerState a' b'
 where
  a' = a `plusWord#` v
  b' = b `plusWord#` a'
{-# INLINE advanceNoMod #-}

-- The block must be less than 5552 bytes long in this case
advanceAdlerLimited :: AdlerState -> S.ByteString -> AdlerState
advanceAdlerLimited !state !bl = AdlerState stateA' stateB'
 where
  !(AdlerState stateA stateB) = S.foldl' advanceNoMod state bl
  stateA' = stateA `remWord#` 65521##
  stateB' = stateB `remWord#` 65521##

advanceAdlerBlock :: AdlerState -> S.ByteString -> AdlerState
advanceAdlerBlock !state !bl
  | S.length bl == 0 = state
  | S.length bl == 1 = advanceAdler state (S.head bl)
  | S.length bl < 5552 = advanceAdlerLimited state bl 
  | otherwise = advanceAdlerBlock (advanceAdlerBlock state first5551) rest
 where
  (!first5551, !rest) = S.splitAt 5551 bl

finalizeAdler :: AdlerState -> Word32
finalizeAdler (AdlerState a b) = high .|. low
  where
   high = (W32# b) `shiftL` 16
   low = W32# a