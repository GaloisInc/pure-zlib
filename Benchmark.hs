import Codec.Compression.Zlib(ZlibDecoder(..), decompressIncremental)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Prelude hiding (readFile, writeFile)
import Criterion.Main

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
        bench tc $ whnf (runDecompression (L.toChunks zbstr) goldbstr) decompressIncremental
  ]
  where
    getFiles tc = do
      zbstr    <- L.readFile $ "test/test-cases/" ++ tc ++ ".z"
      goldbstr <- L.readFile $ "test/test-cases/" ++ tc ++ ".gold"
      pure (zbstr, goldbstr)

runDecompression :: [S.ByteString] -> L.ByteString -> ZlibDecoder -> ()
runDecompression ls real decoder =
  case decoder of
    Done | not (null ls) -> error "ERROR: Finished decompression with data left."
    Done | not (L.null real) -> error "ERROR: Did not completely decompress file."
    Done | otherwise -> ()
    DecompError e -> error ("ERROR: " ++ show e)
    NeedMore f | (x:rest) <- ls -> runDecompression rest real (f x)
               | otherwise      -> error "ERROR: Ran out of data mid-decompression."
    Chunk c m ->
      let (realfirst, realrest) = L.splitAt (L.length c) real
      in if realfirst == c
           then runDecompression ls realrest m
           else error "Mismatch in decompression"
