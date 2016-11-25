import Codec.Compression.Zlib(ZlibDecoder(..), decompressIncremental)
import Control.Monad(unless)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import Prelude hiding (readFile, writeFile)

main :: IO ()
main =
  do zbstr    <- L.readFile "test/test-cases/tor-list.z"
     goldbstr <- L.readFile "test/test-cases/tor-list.gold"
     before   <- getCurrentTime
     runDecompression (L.toChunks zbstr) goldbstr decompressIncremental
     after    <- getCurrentTime
     putStrLn ("Decompression took " ++ show (diffUTCTime after before))

runDecompression :: [S.ByteString] -> L.ByteString -> ZlibDecoder -> IO ()
runDecompression ls real decoder =
  case decoder of
    Done ->
      do unless (null ls) $
           fail "ERROR: Finished decompression with data left."
         unless (L.null real) $
           fail "ERROR: Did not completely decompress file."
         return ()
    DecompError e ->
      fail ("ERROR: " ++ show e)
    NeedMore f | (x:rest) <- ls -> runDecompression rest real (f x)
               | otherwise      ->
      fail "ERROR: Ran out of data mid-decompression."
    Chunk c m ->
      let (realfirst, realrest) = L.splitAt (L.length c) real
      in if realfirst == c
           then runDecompression ls realrest m
           else fail "Mismatch in decompression"
