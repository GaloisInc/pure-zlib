import qualified PureZlib
import qualified CZlib
import qualified CZlib.Internal as CZlibIncremental

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Prelude hiding (readFile, writeFile)
import Criterion.Main
import Control.Exception
import Control.Monad.ST.Lazy
import Control.Monad (unless)

testCases :: [String]
testCases = [ "randtest1", "randtest2", "randtest3",
              "rfctest1",  "rfctest2",  "rfctest3",
              "zerotest1", "zerotest2", "zerotest3",
              "tor-list" ]

main :: IO ()
main = defaultMain
  [
    bgroup "decompression" $
      flip fmap testCases $
        \tc -> env (getFiles tc) $
          \ ~(zbstr, goldbstr) ->
            bgroup tc [
              bgroup "normal" [
                bench "pure-zlib" $ whnf (decompressPure zbstr) goldbstr
                , bench "zlib" $ whnf (decompressC zbstr) goldbstr
              ]
              , bgroup "incremental" [
                bench "pure-zlib" $ whnf (decompressIncrementalPure (L.toChunks zbstr)) goldbstr
                , bench "zlib" $ whnf (decompressIncrementalC (L.toChunks zbstr)) goldbstr
              ]
            ]
  ]
  where
    getFiles tc = do
      zbstr    <- L.readFile $ "test/test-cases/" ++ tc ++ ".z"
      goldbstr <- L.readFile $ "test/test-cases/" ++ tc ++ ".gold"
      pure (zbstr, goldbstr)

decompressPure :: L.ByteString -> L.ByteString -> ()
decompressPure ls real = case PureZlib.decompress ls of
  Left e -> throw e
  Right decompressed -> if real == decompressed then () else error "Mismatch in decompression"

decompressIncrementalPure :: [S.ByteString] -> L.ByteString ->  ()
decompressIncrementalPure = go PureZlib.decompressIncremental
  where
    go decoder ls real =
      case decoder of
        PureZlib.Done | not (null ls) -> error "ERROR: Finished decompression with data left."
        PureZlib.Done | not (L.null real) -> error "ERROR: Did not completely decompress file."
        PureZlib.Done | otherwise -> ()
        PureZlib.DecompError e -> error ("ERROR: " ++ show e)
        PureZlib.NeedMore f | (x:rest) <- ls -> go (f x) rest real
                  | otherwise      -> error "ERROR: Ran out of data mid-decompression."
        PureZlib.Chunk c m ->
          let (realfirst, realrest) = L.splitAt (L.length c) real
          in if realfirst == c
              then go m ls realrest
              else error "Mismatch in decompression"

decompressC :: L.ByteString -> L.ByteString -> ()
decompressC input real =
  let decompressed =  CZlib.decompress input
  in if real == decompressed then () else error "Mismatch in decompression"

decompressIncrementalC :: [S.ByteString] -> L.ByteString -> ()
decompressIncrementalC ls real = runST $ go (CZlibIncremental.decompressST CZlibIncremental.zlibFormat CZlibIncremental.defaultDecompressParams) ls real
  where
    go :: CZlibIncremental.DecompressStream (ST s) -> [S.ByteString] -> L.ByteString -> ST s ()
    go decoder ls real = case decoder of
      CZlibIncremental.DecompressInputRequired f
        | (x:rest) <- ls -> do
          next <- f x
          go next rest real
        | otherwise -> error "ERROR: Ran out of data mid-decompression."
      CZlibIncremental.DecompressOutputAvailable c kont -> do
          let (realfirst, realrest) = L.splitAt (fromIntegral $ S.length c) real
          unless (L.toStrict realfirst == c) $ error "Mismatch in decompression"
          next <- kont
          go next ls realrest
      CZlibIncremental.DecompressStreamEnd leftovers
        | not (S.null leftovers) -> error "ERROR: Finished decompression with data left."
        | not (L.null real) -> error "ERROR: Did not completely decompress file."
        | otherwise -> pure ()
      CZlibIncremental.DecompressStreamError e -> error ("ERROR: " ++ show e)
