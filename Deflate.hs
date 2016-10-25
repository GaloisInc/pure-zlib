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
                     Left  err -> putStrLn (show err)
                     Right bs  -> writeFile (take (length ifile - 2) ifile) bs
           else putStrLn "Unexpected file name."
       _ ->
         putStrLn "USAGE: deflate [filename]"
