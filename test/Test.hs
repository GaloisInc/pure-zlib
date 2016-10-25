import Codec.Compression.Zlib
import Codec.Compression.Zlib.Deflate
import Data.ByteString.Lazy(readFile)
import Data.List(last, isPrefixOf)
import Prelude hiding (readFile)
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

-- -----------------------------------------------------------------------------

rfcSimpleTestLengths :: [(Char, Int)]
rfcSimpleTestLengths = [
    ('A', 3)
  , ('B', 3)
  , ('C', 3)
  , ('D', 3)
  , ('E', 3)
  , ('F', 2)
  , ('G', 4)
  , ('H', 4)
  ]

rfcSimpleTestResults :: [(Char, Int, Int)]
rfcSimpleTestResults = [
    ('A', 3, 2)  --  010
  , ('B', 3, 3)  --  011
  , ('C', 3, 4)  --  100
  , ('D', 3, 5)  --  101
  , ('E', 3, 6)  --  110
  , ('F', 2, 0)  --   00
  , ('G', 4, 14) -- 1110
  , ('H', 4, 15) -- 1111
  ]

fixedHuffmanLengths :: [(Int, Int)]
fixedHuffmanLengths =
  ([(x, 8) | x <- [0   .. 143]] ++
   [(x, 9) | x <- [144 .. 255]] ++
   [(x, 7) | x <- [256 .. 279]] ++
   [(x, 8) | x <- [280 .. 287]])

fixedHuffmanResults :: [(Int, Int, Int)]
fixedHuffmanResults =
  ([(fst x, 8, snd x) | x <- zip [0  ..143] [48 ..191]] ++ --  00110000 through  10111111
   [(fst x, 9, snd x) | x <- zip [144..255] [400..511]] ++ -- 110010000 through 111111111
   [(fst x, 7, snd x) | x <- zip [256..279] [0  .. 23]] ++ --   0000000 through   0010111
   [(fst x, 8, snd x) | x <- zip [280..287] [192..199]])   --  11000000 through  11000111

-- -----------------------------------------------------------------------------

testCases :: [FilePath]
testCases = [ "randtest1", "randtest2", "randtest3",
              "rfctest1",  "rfctest2",  "rfctest3",
              "zerotest1", "zerotest2", "zerotest3" ]

buildGoldTestCases :: IO TestTree
buildGoldTestCases =
  do trees <- mapM buildGoldTest testCases
     return (testGroup "Decompression Tests" trees)

buildGoldTest :: FilePath -> IO TestTree
buildGoldTest test =
  do let compressedFile = "test" </> "test-cases" </> test <.> "z"
         goldFile       = "test" </> "test-cases" </> test <.> "gold"
     compressedBStr <- readFile compressedFile
     goldBStr       <- readFile goldFile
     return (testCase (toTestCaseName test)
              (assertEqual test (Right goldBStr) (decompress compressedBStr)))

toTestCaseName :: FilePath -> String
toTestCaseName fpath = prefix ++ suffix
 where
  prefix | "zero" `isPrefixOf` fpath = "Zero test #"
         | "rand" `isPrefixOf` fpath = "Random test #"
         | "rfc"  `isPrefixOf` fpath = "RFC test #"
         | otherwise                 = error "Bad test case prefix."
  suffix = [last fpath]

-- -----------------------------------------------------------------------------

zlibTests :: IO TestTree
zlibTests =
  do decompTests <- buildGoldTestCases
     return $ testGroup "DEFLATE / ZLib Algorithm Testing" [
                  testCase "RFC 1951 Code Generation Test"
                    (assertEqual "" (computeCodeValues rfcSimpleTestLengths)
                                    rfcSimpleTestResults)
                , testCase "Fixed Huffman lengths make right tree"
                    (assertEqual "" (computeCodeValues fixedHuffmanLengths)
                                    fixedHuffmanResults)
                , decompTests
                ]

main :: IO ()
main = defaultMain =<< zlibTests

