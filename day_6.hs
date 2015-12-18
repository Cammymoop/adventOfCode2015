import System.IO
import Data.List (foldl')
import Data.Array

-- input parsing
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate s = case dropWhile predicate s of
                      "" -> []
                      s' -> w : splitOn predicate s''
                            where (w, s'') = break predicate s'
splitOnSpace = splitOn (== ' ')

-- Read 2 coordinates seperated by a comma into a tuple
readInt :: String -> Int
readInt = read

firstCoord :: String -> Int
firstCoord = readInt . takeWhile (not . (==) ',')
secondCoord :: String -> Int
secondCoord = readInt . drop 1 . dropWhile (not . (==) ',')

parseCoordPair :: [String] -> ((Int, Int), (Int, Int))
parseCoordPair xs = (strToCoordinates (head xs), strToCoordinates (head $ drop 2 xs)) -- drop 2 because we dont care about "through"
                    where strToCoordinates str = (firstCoord str, secondCoord str)

-- part 1
type LightArray1 = Array (Int, Int) Bool

blankLights1 :: LightArray1
blankLights1 = array ((0, 999), (0, 999)) [((x, y), False) | x <- [0..999], y <- [0..999]]

countLightsOn :: (Integral a) => LightArray1 -> a
countLightsOn = foldr (\x acc -> if x then succ acc else acc) 0 . elems

toggleLights :: ((Int, Int), (Int, Int)) -> LightArray1 -> LightArray1
toggleLights ((minX, minY), (maxX, maxY)) lights = flip (//) [((x, y), not $ lights!(x, y)) | x <- [minX..maxX], y <- [minY..maxY]] 

setLights :: Bool -> ((Int, Int), (Int, Int)) -> LightArray1 -> LightArray1
setLights new ((minX, minY), (maxX, maxY)) = flip (//) [((x, y), new) | x <- [minX..maxX], y <- [minY..maxY]]

setLightsOn  = setLights True
setLightsOff = setLights False

applyInstruction :: String -> LightArray1 -> LightArray1
applyInstruction = parse . splitOnSpace
                   where parse (x:xs) = case x of "turn"   -> parse xs
                                                  "toggle" -> toggleLights $ parseCoordPair xs
                                                  "on"     -> setLightsOn $ parseCoordPair xs
                                                  "off"    -> setLightsOff $ parseCoordPair xs

doLightShow :: [String] -> LightArray1
doLightShow = foldl' (flip applyInstruction) blankLights1

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_6" ReadMode
  rawInput <- hGetContents inFile
  let input = lines rawInput
  print $ countLightsOn $ doLightShow input
  
