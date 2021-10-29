{-# LANGUAGE RankNTypes #-}

import Codec.Compression.Zlib (ZlibDecoder (..), decompressIncremental)
import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.List (isSuffixOf)
import GHC.IO (stToIO)
import GHC.Prim (RealWorld)
import GHC.ST (ST)
import System.Environment (getArgs)
import System.IO (Handle, IOMode (..), hClose, openFile)
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ifile] ->
      if ".z" `isSuffixOf` ifile
        then do
          bstr <- L.readFile ifile
          let outname = take (length ifile - 2) ifile
          hndl <- openFile outname WriteMode
          runDecompression hndl (L.toChunks bstr) decompressIncremental
        else putStrLn "Unexpected file name."
    _ ->
      putStrLn "USAGE: deflate [filename]"

runDecompression :: Handle -> [S.ByteString] -> ST RealWorld (ZlibDecoder RealWorld) -> IO ()
runDecompression hndl ls decoder = do
  nextState <- stToIO decoder
  case nextState of
    Done -> do
      unless (null ls) $
        putStrLn "WARNING: Finished decompression with data left."
      hClose hndl
    DecompError e -> do
      putStrLn ("ERROR: " ++ show e)
      hClose hndl
    NeedMore f
      | (x : rest) <- ls -> runDecompression hndl rest (f x)
      | otherwise -> do
        putStrLn "ERROR: Ran out of data mid-decompression."
        hClose hndl
    Chunk c m -> do
      S.hPut hndl c
      runDecompression hndl ls m
