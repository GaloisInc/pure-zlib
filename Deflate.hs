import Codec.Compression.Zlib(decompress)
import Data.ByteString.Lazy(readFile, writeFile)
import Data.List(isSuffixOf)
import Prelude hiding (readFile, writeFile)
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     case args of
       [ifile] ->
         if ".z" `isSuffixOf` ifile
           then do bstr <- readFile ifile
                   case decompress bstr of
                     Nothing -> putStrLn "Decompression failure."
                     Just o  -> writeFile (take (length ifile - 2) ifile) o
           else putStrLn "Unexpected file name."
       _ ->
         putStrLn "USAGE: deflate [filename]"
