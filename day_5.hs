import System.IO
import qualified Data.Set as Set
import Data.List (foldl')

-- vowel check
vowels :: Set.Set Char
vowels = Set.fromList "aeiou"

isVowel :: Char -> Bool
isVowel = flip Set.member vowels 

hasThreeVowels :: String -> Bool
hasThreeVowels = (3 <=) . (foldr (\x acc -> if isVowel x then acc + 1 else acc) 0)

-- check for doubles
sameAsPrev :: Char -> (Maybe Char, Bool) -> (Maybe Char, Bool)
sameAsPrev _ (_, True)          = (Nothing, True)
sameAsPrev c (Nothing, False)   = (Just c, False)
sameAsPrev c (Just prev, False) = if c == prev then (Nothing, True) else (Just c, False)

hasDouble :: String -> Bool
hasDouble = snd . (foldr sameAsPrev (Nothing, False))

-- invalid pair check
invalids :: Set.Set (Char, Char)
invalids = Set.fromList $ zip "acpx" "bdqy"

isInvalid :: Char -> (Maybe Char, Bool) -> (Maybe Char, Bool)
isInvalid _ (_, True)          = (Nothing, True)
isInvalid c (Nothing, False)   = (Just c, False)
isInvalid c (Just prev, False) = if invalidPair (c, prev) then (Nothing, True) else (Just c, False)
    where invalidPair = flip Set.member invalids 
-- prev comes after c because I'm using a right fold

hasInvalid :: String -> Bool
hasInvalid = snd . (foldr isInvalid (Nothing, False))

-- part 1
isNice :: String -> Bool
isNice str = not (hasInvalid str) && hasDouble str && hasThreeVowels str

countNice :: (Integral a) => [String] -> a
countNice = Data.List.foldl' (\acc str -> if isNice str then acc + 1 else acc) 0


-- part 2 stuff

-- Coming Soon!!!!

-- hasSandwhich :: String -> Bool

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_5" ReadMode
  rawInput <- hGetContents inFile
  let input = lines rawInput
  print $ countNice input
  
