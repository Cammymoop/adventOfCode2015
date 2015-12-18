import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5     -- pureMD5 package
import Codec.Binary.UTF8.String -- uft8-string package

input = "iwrupvqb"

-- MD5 from a string
stringMD5 :: String -> String
stringMD5 = show . md5 . BL.pack . encode

md5StartsWith :: String -> String -> Bool
md5StartsWith str = (.) (str==) $ (take $ length str) . stringMD5

-- part 1
firstWith5z = firstInt $ md5StartsWith "00000"

-- part 2
firstWith6z = firstInt $ md5StartsWith "000000"

firstInt :: (String -> Bool) -> String -> Int -> Int
firstInt testFunc str i = case testFunc (str ++ (show i)) of False -> firstInt testFunc str (i+1)
                                                             True -> i

main :: IO ()
main = do 
  print $ firstWith5z input 1
  print $ firstWith6z input 1
