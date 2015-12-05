import System.IO

-- Input Parsing
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate s =  case dropWhile predicate s of
                      "" -> []
                      s' -> w : splitOn predicate s''
                            where (w, s'') = break predicate s'
splitOnX = splitOn (== 'x')

toInts :: [String] -> [Int]
toInts = map read

inputReadFunc = (map (toInts . splitOnX)) . lines
-- End input Parsing

-- convenience
interlaceXYZ (x:y:z:_) = [[x, y], [y, z], [x, z]]

combine :: (Num a) => (b -> a) -> (b -> a) -> (b -> a)
combine f f' x = (f x) + (f' x)

-- part 1
areas :: (Num a) => [a] -> [a]
areas = (map product) . interlaceXYZ

surfaceArea :: (Num a) => [a] -> a
surfaceArea = sum . (map (2*)) . areas

extraWrapping :: (Ord a, Num a) => [a] -> a
extraWrapping = minimum . areas

totalWrapping :: (Ord a, Num a) => [a] -> a
totalWrapping = combine surfaceArea extraWrapping

-- part 2
smallestPerimeter :: (Ord a, Num a) => [a] -> a
smallestPerimeter = (*2) . minimum . (map sum) . interlaceXYZ

bowLength :: (Num a) => [a] -> a
bowLength = product -- Re-named for readability

totalRibbon :: (Ord a, Num a) => [a] -> a
totalRibbon = combine smallestPerimeter bowLength

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_2" ReadMode
  rawInput <- hGetContents inFile
  let input = inputReadFunc rawInput
  print $ sum $ map totalWrapping input
  print $ sum $ map totalRibbon input

