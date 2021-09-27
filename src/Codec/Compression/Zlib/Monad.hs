{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE Rank2Types                 #-}
module Codec.Compression.Zlib.Monad(
         DeflateM
       , runDeflateM
       , ZlibDecoder(..)
       , raise
       , DecompressionError(..)
         -- * Getting data from the input stream.
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

import           Codec.Compression.Zlib.Adler32(AdlerState, initialAdlerState,
                                                advanceAdler, advanceAdlerBlock,
                                                finalizeAdler)
import           Codec.Compression.Zlib.HuffmanTree(HuffmanTree, advanceTree,
                                                    AdvanceResult(..))
import           Codec.Compression.Zlib.OutputWindow(OutputWindow, emptyWindow,
                                                     emitExcess, addByte,
                                                     addChunk, addOldChunk,
                                                     finalizeWindow)
import           Control.Exception(Exception)
import           Data.Bits(Bits(..))
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Int(Int64)
import           Data.Typeable(Typeable)
import           Data.Word(Word32, Word16, Word8)
import GHC.ST(ST)
import           Prelude()
import           Prelude.Compat

data DecompressionState s = DecompressionState {
       dcsNextBitNo     :: !Int
     , dcsCurByte       :: !Word8
     , dcsAdler32       :: !AdlerState
     , dcsInput         :: !S.ByteString
     , dcsOutput        :: !(OutputWindow s)
     }

instance Show (DecompressionState s) where
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

newtype DeflateM s a = DeflateM {
    unDeflateM :: DecompressionState s ->
                  (DecompressionState s -> a -> ST s (ZlibDecoder s)) ->
                  ST s (ZlibDecoder s)
  }

instance Applicative (DeflateM s) where
  pure  x = DeflateM (\ s k -> k s x)

  f <*> x = DeflateM $ \ s1 k ->
     unDeflateM f s1 $ \ s2 g ->
     unDeflateM x s2 $ \ s3 y -> k s3 (g y)

  m *> n = DeflateM $ \ s1 k ->
    unDeflateM m s1 $ \ s2 _ -> unDeflateM n s2 k

  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}


instance Functor (DeflateM s) where
  fmap f m = DeflateM (\s k -> unDeflateM m s (\s' a -> k s' (f a)))
  {-# INLINE fmap #-}

instance Monad (DeflateM s) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  m >>= f = DeflateM $ \ s1 k ->
     unDeflateM m s1 $ \ s2 a -> unDeflateM (f a) s2 k

  (>>) = (*>)
  {-# INLINE (>>) #-}

get :: DeflateM s (DecompressionState s)
get = DeflateM (\ s k -> k s s)
{-# INLINE get #-}

set :: DecompressionState s -> DeflateM s ()
set !s = DeflateM (\ _ k -> k s ())
{-# INLINE set #-}

raise :: DecompressionError -> DeflateM s a
raise e = DeflateM (\ _ _ -> return (DecompError e))
{-# INLINE raise #-}

liftST :: ST s a -> DeflateM s a
liftST action = DeflateM $ \ s k -> do
  res <- action
  k s res

-- -----------------------------------------------------------------------------

data ZlibDecoder s
   = NeedMore (S.ByteString -> ST s (ZlibDecoder s))
   | Chunk S.ByteString (ST s (ZlibDecoder s))
   | Done
   | DecompError DecompressionError

runDeflateM :: DeflateM s () -> ST s (ZlibDecoder s)
runDeflateM m = do
  window <- emptyWindow
  let initialState = DecompressionState {
    dcsNextBitNo = 8
  , dcsCurByte = 0
  , dcsAdler32 = initialAdlerState 
  , dcsInput = S.empty 
  , dcsOutput = window
  }
  unDeflateM m initialState (\ _ _ -> return Done)
{-# INLINE runDeflateM #-}

-- -----------------------------------------------------------------------------

getNextChunk :: DeflateM s ()
getNextChunk = DeflateM $ \ st k -> return (NeedMore (loadChunk st k))
 where
  loadChunk ::
    DecompressionState s ->
    (DecompressionState s -> () -> ST s (ZlibDecoder s)) ->
    S.ByteString ->
    ST s (ZlibDecoder s)
  loadChunk st k bstr =
    case S.uncons bstr of
      Nothing -> return (NeedMore (loadChunk st k))
      Just (nextb, rest) ->
        k st{ dcsNextBitNo = 0, dcsCurByte = nextb, dcsInput = rest } ()

{-# SPECIALIZE nextBits :: Int -> DeflateM s Word8 #-}
{-# SPECIALIZE nextBits :: Int -> DeflateM s Int   #-}
{-# SPECIALIZE nextBits :: Int -> DeflateM s Int64 #-}
{-# INLINE nextBits #-}
nextBits :: (Num a, Bits a) => Int -> DeflateM s a
nextBits x = nextBits' x 0 0

{-# SPECIALIZE nextBits' :: Int -> Int -> Word8 -> DeflateM s Word8 #-}
{-# SPECIALIZE nextBits' :: Int -> Int -> Int   -> DeflateM s Int   #-}
{-# SPECIALIZE nextBits' :: Int -> Int -> Int64 -> DeflateM s Int64 #-}
{-# INLINE nextBits' #-}
nextBits' :: (Num a, Bits a) => Int -> Int -> a -> DeflateM s a
nextBits' !x' !shiftNum !acc
  | x' == 0       = return acc
  | otherwise     =
      do dcs <- get
         case dcsNextBitNo dcs of
           8 -> case S.uncons (dcsInput dcs) of
                  Nothing ->
                    do getNextChunk 
                       nextBits' x' shiftNum acc
                  Just (nextb, rest) ->
                    do set dcs{dcsNextBitNo=0,dcsCurByte=nextb,dcsInput=rest}
                       nextBits' x' shiftNum acc
           nextBitNo ->
             do let !myBits = min x' (8 - nextBitNo)
                    !base   = dcsCurByte dcs `shiftR` nextBitNo
                    !mask   = complement (0xFF `shiftL` myBits)
                    !res    = fromIntegral (base .&. mask)
                    !acc'   = acc .|. (res `shiftL` shiftNum)
                set dcs { dcsNextBitNo=nextBitNo + myBits }
                nextBits' (x' - myBits) (shiftNum + myBits) acc'

nextByte :: DeflateM s Word8
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

nextWord16 :: DeflateM s Word16
nextWord16 =
  do low  <- fromIntegral `fmap` nextByte
     high <- fromIntegral `fmap` nextByte
     return ((high `shiftL` 8) .|. low)

nextWord32 :: DeflateM s Word32
nextWord32 =
  do a <- fromIntegral `fmap` nextByte
     b <- fromIntegral `fmap` nextByte
     c <- fromIntegral `fmap` nextByte
     d <- fromIntegral `fmap` nextByte
     return ((a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d)

nextBlock :: Integral a => a -> DeflateM s L.ByteString
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
            raise (FormatError "Can't get a block on a non-byte boundary.")
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

nextCode :: Show a => HuffmanTree a -> DeflateM s a
nextCode tree =
  do b <- nextBits 1
     case advanceTree b tree of
       AdvanceError str -> raise (HuffmanTreeError str)
       NewTree tree'    -> nextCode tree'
       Result x         -> return x
{-# INLINE nextCode #-}

advanceToByte :: DeflateM s ()
advanceToByte =
  do dcs <- get
     set dcs{ dcsNextBitNo = 8 }

emitByte :: Word8 -> DeflateM s ()
emitByte b =
  do dcs <- get
     output' <- liftST (addByte (dcsOutput dcs) b)
     let adler' = advanceAdler (dcsAdler32 dcs) b
     set dcs{ dcsOutput = output', dcsAdler32 = adler' }
{-# INLINE emitByte #-}

emitBlock :: L.ByteString -> DeflateM s ()
emitBlock b =
  do dcs <- get
     output' <- liftST (addChunk (dcsOutput dcs) b)
     let adler' = L.foldlChunks advanceAdlerBlock (dcsAdler32 dcs) b
     set dcs { dcsOutput  = output', dcsAdler32 = adler' }

emitPastChunk :: Int -> Int64 -> DeflateM s ()
emitPastChunk dist len =
  do dcs <- get
     (output', newChunk) <- liftST (addOldChunk (dcsOutput dcs) dist len)
     set dcs { dcsOutput = output'
             , dcsAdler32 = advanceAdlerBlock (dcsAdler32 dcs) newChunk }
{-# INLINE emitPastChunk #-}

finalAdler :: DeflateM s Word32
finalAdler = (finalizeAdler . dcsAdler32) <$> get

moveWindow :: DeflateM s ()
moveWindow =
  do dcs <- get
     possibleExcess <- liftST (emitExcess (dcsOutput dcs))
     case possibleExcess of
       Nothing ->
         return ()
       Just (builtChunk, output') ->
         do set dcs{ dcsOutput = output' }
            publish builtChunk

finalize :: DeflateM s ()
finalize =
  do dcs <- get
     lastChunk <- liftST (finalizeWindow (dcsOutput dcs))
     publish lastChunk

{-# INLINE publish #-}
publish :: S.ByteString -> DeflateM s ()
publish bstr = DeflateM $ \ st k ->
  return (Chunk bstr (k st ()))
