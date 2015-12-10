startingInput = "3113322113"

countRepeatingChars :: (Integral a, Show a) => Char -> (Maybe Char, a, String) -> (Maybe Char, a, String)
countRepeatingChars c (Nothing, _, _)        = (Just c, 1, []) 
countRepeatingChars c (Just curChar, curCount, str) = if c == curChar
                                                      then (Just curChar, succ curCount, str)
						      else (Just c, 1, (show curCount) ++ curChar:str)

finalPrepend :: (Integral a, Show a) => (Maybe Char, a, String) -> String
finalPrepend (Just c, count, str) = (show count) ++ c:str

elfRead :: String -> String
elfRead = finalPrepend . foldr countRepeatingChars (Nothing, 0, [])

repeatElf :: (Integral a) => a -> String -> String
repeatElf 1 = elfRead 
repeatElf n = repeatElf (n-1) . elfRead

main :: IO ()
main = do
  print $ length $ repeatElf 40 startingInput
  print $ length $ repeatElf 50 startingInput
  
