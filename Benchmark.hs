import Codec.Compression.Zlib(ZlibDecoder(..), decompressIncremental)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Prelude hiding (readFile, writeFile)
import Criterion.Main
import GHC.ST(ST, runST)

testCases :: [String]
testCases = [ "randtest1", "randtest2", "randtest3",
              "rfctest1",  "rfctest2",  "rfctest3",
              "zerotest1", "zerotest2", "zerotest3",
              "tor-list" ]

main :: IO ()
main = defaultMain
  [
    bgroup "decompression" $ flip fmap testCases $ \tc ->
      env (getFiles tc) $ \ ~(zbstr, goldbstr) ->
        bench tc $ whnf (runDecompression (L.toChunks zbstr)) goldbstr
  ]
  where
    getFiles tc = do
      zbstr    <- L.readFile $ "test/test-cases/" ++ tc ++ ".z"
      goldbstr <- L.readFile $ "test/test-cases/" ++ tc ++ ".gold"
      pure (zbstr, goldbstr)

runDecompression :: [S.ByteString] -> L.ByteString -> ()
runDecompression ls real = runST $ do
  firstState <- decompressIncremental
  drive ls real firstState

drive ::
  [S.ByteString] ->
  L.ByteString ->
  ZlibDecoder s ->
  ST s ()
drive ls real state =
  case state of
    Done | not (null ls) -> error "ERROR: Finished decompression with data left."
    Done | not (L.null real) -> error "ERROR: Did not completely decompress file."
    Done | otherwise -> return ()
    DecompError e -> error ("ERROR: " ++ show e)
    NeedMore f | (x:rest) <- ls -> do nextState <- f x
                                      drive rest real nextState
               | otherwise      -> error "ERROR: Ran out of data mid-decompression."
    Chunk c m -> do
      let (realfirst, realrest) = L.splitAt (fromIntegral (S.length c)) real
      if realfirst == L.fromStrict c
        then m >>= drive ls realrest
        else error "Mismatch in decompression"
