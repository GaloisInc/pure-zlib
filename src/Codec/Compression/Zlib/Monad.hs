{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
module Codec.Compression.Zlib.Monad(
         DeflateM
       , runDeflateM
       , ZlibDecoder(..)
       , raise
       , DecompressionError(..)
         -- * Getting data from the input stream.
       , nextBit
       , nextBits
       , nextByte
       , nextWord16
       , nextWord32
       , nextBlock
       , nextCode
         -- * Aligning
       , advanceToByte
         -- * Emitting data into the output window
       , emitByte
       , emitBlock
       , emitPastChunk
         -- * Getting and publishing output
       , finalAdler
       , moveWindow
       , finalize
       )
 where

import Codec.Compression.Zlib.Adler32
import Codec.Compression.Zlib.HuffmanTree
import Codec.Compression.Zlib.OutputWindow
import Control.Exception(Exception)
import Control.Monad
import Data.Bits
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Typeable
import Data.Word
import Prelude()
import Prelude.Compat

data DecompressionState = DecompressionState {
       dcsNextBitNo     :: !Int
     , dcsCurByte       :: !Word8
     , dcsAdler32       :: !AdlerState
     , dcsInput         :: !S.ByteString
     , dcsOutput        :: !OutputWindow
     }

instance Show DecompressionState where
  show dcs = "DecompressionState<nextBit=" ++ show (dcsNextBitNo dcs) ++ "," ++
             "curByte=" ++ show (dcsCurByte dcs) ++ ",inputLen=" ++
             show (S.length (dcsInput dcs)) ++ ">"

-- -----------------------------------------------------------------------------

data DecompressionError = HuffmanTreeError   String
                        | FormatError        String
                        | DecompressionError String
                        | HeaderError        String
                        | ChecksumError      String
  deriving (Typeable, Eq)

instance Show DecompressionError where
  show x =
    case x of
      HuffmanTreeError   s -> "Huffman tree manipulation error: " ++ s
      FormatError        s -> "Block format error: " ++ s
      DecompressionError s -> "Decompression error: " ++ s
      HeaderError        s -> "Header error: " ++ s
      ChecksumError      s -> "Checksum error: " ++ s

instance Exception DecompressionError

-- -----------------------------------------------------------------------------

data DeflateState a = Running   !DecompressionState !a
                    | Dead      !DecompressionState !DecompressionError
                    | NeedsData !DecompressionState !(S.ByteString -> DeflateM a)
                    | HaveChunk !DecompressionState !S.ByteString !(DeflateM a)

newtype DeflateM a = DeflateM {
    unDeflateM :: DecompressionState -> DeflateState a
  }

instance Applicative DeflateM where
  pure  = return
  (<*>) = ap

instance Functor DeflateM where
  fmap f m = do x <- m
                return (f x)

instance Monad DeflateM where
  {-# INLINE return #-}
  return x = DeflateM $ \ st -> Running st x
  {-# INLINE (>>=) #-}
  m >>= f  = DeflateM $ \ st ->
               case unDeflateM m st of
                 Running   st' x        -> unDeflateM (f x) st'
                 Dead      st' e        -> Dead st' e
                 NeedsData st' fill     -> NeedsData st' (\ b -> fill b >>= f)
                 HaveChunk st' c resume -> HaveChunk st' c (resume >>= f)

get :: DeflateM DecompressionState
get = DeflateM (\ s -> Running s s)

set :: DecompressionState -> DeflateM ()
set st = DeflateM (\ _ -> Running st ())

raise :: DecompressionError -> DeflateM a
raise e = DeflateM (\ st -> Dead st e)

initialState :: DecompressionState
initialState = DecompressionState {
    dcsNextBitNo = 8
  , dcsCurByte   = 0
  , dcsAdler32   = initialAdlerState
  , dcsInput     = S.empty
  , dcsOutput    = emptyWindow
  }

-- -----------------------------------------------------------------------------

data ZlibDecoder = NeedMore (S.ByteString -> ZlibDecoder)
                 | Chunk S.ByteString ZlibDecoder
                 | Done
                 | DecompError DecompressionError

runDeflateM :: DeflateM () -> ZlibDecoder
runDeflateM m = runDeflateM' initialState m

runDeflateM' :: DecompressionState -> DeflateM () -> ZlibDecoder
runDeflateM' st m =
  case unDeflateM m st of
    Running   _   _    -> Done
    Dead      _   e    -> DecompError e
    NeedsData st' f    -> NeedMore (\ bstr -> runDeflateM' st' (f bstr))
    HaveChunk st' c m' -> Chunk c (runDeflateM' st' m')

-- -----------------------------------------------------------------------------

getNextChunk :: DeflateM ()
getNextChunk = DeflateM $ \ st -> NeedsData st loadChunk
 where
  loadChunk bstr =
    case S.uncons bstr of
      Nothing -> getNextChunk
      Just (nextb, rest) ->
        do dcs <- get
           set dcs{ dcsNextBitNo = 0, dcsCurByte = nextb, dcsInput = rest }

{-# INLINE nextBit #-}
nextBit :: DeflateM Bool
nextBit =
  do dcs <- get
     let !nextBitNo = dcsNextBitNo dcs
     if | nextBitNo >  8 -> raise (DecompressionError "Weird bit state")
        | nextBitNo == 8 -> case S.uncons (dcsInput dcs) of
                              Nothing -> getNextChunk >> nextBit
                              Just (nextb, rest) ->
                                do set dcs{ dcsNextBitNo = 0
                                          , dcsCurByte   = nextb
                                          , dcsInput     = rest }
                                   nextBit
        | otherwise      -> do let !v = dcsCurByte dcs `testBit` nextBitNo
                               set $ dcs{ dcsNextBitNo = nextBitNo + 1 }
                               return v

nextBits :: (Num a, Bits a) => Int -> DeflateM a
nextBits x
 | x < 1     = error "nextBits called with x < 1"
 | x == 1    = toNum `fmap` nextBit
 | otherwise = do cur  <- toNum `fmap` nextBit
                  rest <- nextBits (x - 1)
                  return ((rest `shiftL` 1) .|. cur)
 where
  toNum False = 0
  toNum True  = 1

nextByte :: DeflateM Word8
nextByte =
  do dcs <- get
     if | dcsNextBitNo dcs == 0 -> do set dcs{ dcsNextBitNo = 8 }
                                      return (dcsCurByte dcs)
        | dcsNextBitNo dcs /= 8 -> nextBits 8 -- we're not aligned. sigh.
        | otherwise             -> case S.uncons (dcsInput dcs) of
                                     Nothing -> getNextChunk >> nextByte
                                     Just (nextb, rest) ->
                                       do set dcs{ dcsNextBitNo = 8,
                                                   dcsCurByte   = nextb,
                                                   dcsInput     = rest }
                                          return nextb

nextWord16 :: DeflateM Word16
nextWord16 =
  do low  <- fromIntegral `fmap` nextByte
     high <- fromIntegral `fmap` nextByte
     return ((high `shiftL` 8) .|. low)

nextWord32 :: DeflateM Word32
nextWord32 =
  do a <- fromIntegral `fmap` nextByte
     b <- fromIntegral `fmap` nextByte
     c <- fromIntegral `fmap` nextByte
     d <- fromIntegral `fmap` nextByte
     return ((a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d)

nextBlock :: Integral a => a -> DeflateM L.ByteString
nextBlock amt =
  do dcs <- get
     if | dcsNextBitNo dcs == 0 ->
            do let startByte = dcsCurByte dcs
               set dcs{ dcsNextBitNo = 8 }
               rest <- nextBlock (amt - 1)
               return (L.cons startByte rest)
        | dcsNextBitNo dcs == 8 ->
            getBlock (fromIntegral amt) (dcsInput dcs)
        | otherwise             ->
            fail "Can't get a block on a non-byte boundary."
 where
  getBlock len bstr
    | len < S.length bstr = do let (mine, rest) = S.splitAt len bstr
                               dcs <- get
                               set dcs{ dcsNextBitNo = 8, dcsInput = rest }
                               return (L.fromStrict mine)
    | S.null bstr         = do getNextChunk
                               dcs <- get
                               let byte1 = dcsCurByte dcs
                               rest <- getBlock (len - 1) (dcsInput dcs)
                               return (L.cons byte1 rest)
    | otherwise           = do rest <- getBlock (len - S.length bstr) S.empty
                               return (L.fromStrict bstr `L.append` rest)

nextCode :: Show a => HuffmanTree a -> DeflateM a
nextCode tree =
  do b <- nextBit
     case advanceTree b tree of
       AdvanceError str -> raise (HuffmanTreeError str)
       NewTree tree'    -> nextCode tree'
       Result x         -> return x

advanceToByte :: DeflateM ()
advanceToByte =
  do dcs <- get
     set dcs{ dcsNextBitNo = 8 }

emitByte :: Word8 -> DeflateM ()
emitByte b =
  do dcs <- get
     set dcs{ dcsOutput  = dcsOutput dcs `addByte` b
            , dcsAdler32 = advanceAdler (dcsAdler32 dcs) b }

emitBlock :: L.ByteString -> DeflateM ()
emitBlock b =
  do dcs <- get
     set dcs { dcsOutput  = dcsOutput dcs `addChunk` b
             , dcsAdler32 = L.foldl advanceAdler (dcsAdler32 dcs) b }

emitPastChunk :: Int -> Int64 -> DeflateM ()
emitPastChunk dist len =
  do dcs <- get
     let (output', builtChunks, newChunk) = addOldChunk (dcsOutput dcs) dist len
     set dcs { dcsOutput = output'
             , dcsAdler32 = L.foldl advanceAdler (dcsAdler32 dcs) newChunk }
     publishLazy builtChunks

finalAdler :: DeflateM Word32
finalAdler = (finalizeAdler . dcsAdler32) `fmap` get

moveWindow :: DeflateM ()
moveWindow =
  do dcs <- get
     let (builtChunks, output') = adjustWindow (dcsOutput dcs)
     set dcs{ dcsOutput = output' }
     publishLazy builtChunks

finalize :: DeflateM ()
finalize =
  do dcs <- get
     publishLazy (finalizeWindow (dcsOutput dcs))

publishLazy :: L.ByteString -> DeflateM ()
publishLazy lbstr = go (L.toChunks lbstr)
 where
  go []       = return ()
  go (c:rest) = DeflateM $ \ st -> HaveChunk st c (go rest)
