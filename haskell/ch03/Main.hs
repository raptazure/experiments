-- removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- typeclasses: Eq, Ord, Show, Read, Enum, Bounded, Num, Integral, Floating...
-- read "[1,2,3,4]" :: [Int]
-- [LT .. GT] 
-- maxBound :: (Bool, Int, Char) 
-- fromIntegral :: (Num b, Integral a) => a -> b
-- fromIntegral (length [1,2,3,4]) + 3.2