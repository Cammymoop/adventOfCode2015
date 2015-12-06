import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

-- read input
readDirection :: (Num a) => Char -> ((a, a) -> (a, a))
readDirection c = case c of '<' -> \(x, y) -> (x+1, y)
                            '>' -> \(x, y) -> (x-1, y)
                            'v' -> \(x, y) -> (x, y+1)
                            '^' -> \(x, y) -> (x, y-1)
                            otherwise -> \a -> a -- Invalid or end of file

-- part 1
allHousesVisited :: (Num a, Ord a) => ((a, a), Set (a, a)) -> Char -> ((a, a), Set (a, a))
allHousesVisited (pos, visited) c = (newPos, Set.insert newPos visited) where newPos = readDirection c $ pos

-- part 2
allHousesVisited2 :: (Num a, Ord a) => ([(a, a)], Set (a, a)) -> Char -> ([(a, a)], Set (a, a))
allHousesVisited2 (pos:otherPos, visited) c = (positions, Set.insert newPos visited)
    where newPos = readDirection c $ pos
          positions = otherPos ++ [newPos]

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_3" ReadMode
  rawInput <- hGetContents inFile
  print $ Set.size . snd $ foldl allHousesVisited ((0, 0), Set.empty) rawInput
  print $ Set.size . snd $ foldl allHousesVisited2 ([(0, 0), (0, 0)], Set.empty) rawInput
