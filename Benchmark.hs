import qualified PureZlib
import qualified CZlib
import qualified CZlib.Internal as CZlibIncremental

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Prelude hiding (readFile, writeFile)
import Criterion.Main
import Control.Monad.ST.Lazy

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
          \ ~(zbstr, _) ->
            bgroup tc [
              bgroup "normal" [
                bench "pure-zlib" $ whnf PureZlib.decompress zbstr
                , bench "zlib" $ whnf CZlib.decompress zbstr
              ]
              , bgroup "incremental" [
                bench "pure-zlib" $ whnf decompressIncrementalPure zbstr
                , bench "zlib" $ whnf decompressIncrementalC zbstr
              ]
            ]
  ]
  where
    getFiles tc = do
      zbstr    <- L.readFile $ "test/test-cases/" ++ tc ++ ".z"
      goldbstr <- L.readFile $ "test/test-cases/" ++ tc ++ ".gold"
      pure (zbstr, goldbstr)

decompressIncrementalPure :: L.ByteString -> L.ByteString
decompressIncrementalPure input = go PureZlib.decompressIncremental (L.toChunks input) []
  where
    go decoder ls chunks =
      case decoder of
        PureZlib.Done | not (null ls) -> error "ERROR: Finished decompression with data left."
        PureZlib.Done | otherwise -> L.fromChunks $ reverse chunks
        PureZlib.DecompError e -> error ("ERROR: " ++ show e)
        PureZlib.NeedMore f
          | (x:rest) <- ls -> go (f x) rest chunks
          | otherwise      -> error "ERROR: Ran out of data mid-decompression."
        PureZlib.Chunk c m -> go m ls (L.toStrict c:chunks)

decompressIncrementalC :: L.ByteString -> L.ByteString
decompressIncrementalC input = runST $ go (CZlibIncremental.decompressST CZlibIncremental.zlibFormat CZlibIncremental.defaultDecompressParams) (L.toChunks input) []
  where
    go :: CZlibIncremental.DecompressStream (ST s) -> [S.ByteString] -> [S.ByteString] -> ST s L.ByteString
    go decoder ls chunks = case decoder of
      CZlibIncremental.DecompressInputRequired f
        | (x:rest) <- ls -> do
          next <- f x
          go next rest chunks
        | otherwise -> error "ERROR: Ran out of data mid-decompression."
      CZlibIncremental.DecompressOutputAvailable c kont -> do
          next <- kont
          go next ls (c:chunks)
      CZlibIncremental.DecompressStreamEnd leftovers
        | not (S.null leftovers) -> error "ERROR: Finished decompression with data left."
        | otherwise -> pure $ L.fromChunks $ reverse chunks
      CZlibIncremental.DecompressStreamError e -> error ("ERROR: " ++ show e)
