import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- import Data.List (nub,sort)
-- import Data.List hiding (nub)
-- import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- intersperse 0 [1,2,3,4,5,6]
-- intercalate " " ["hey","there","guys"]
-- transpose [[1,2,3],[4,5,6],[7,8,9]]
-- map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
-- concat [[3,4,5],[2,3,4],[2,1,1]]
-- concatMap (replicate 4) [1..3]
-- and $ map (>4) [5,6,7,8]
-- or $ map (==4) [2,3,4,5,6,1]
-- all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
-- any (==4) [2,3,5,6,1,4]
-- take 10 $ iterate (* 2) 1
-- splitAt 3 "heyman"
-- let (a,b) = splitAt 3 "foobar" in b ++ a
-- takeWhile (/=' ') "This is a sentence"
-- sum $ takeWhile (<10000) $ map (^3) [1..]
-- dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
-- break (==4) [1,2,3,4,5,6,7]
-- span (/=4) [1,2,3,4,5,6,7]
-- sort "This will be sorted soon"
-- group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- inits "w00t"
-- tails "w00t"
-- let w = "w00t" in zip (inits w) (tails w)
-- "cat" `isInfixOf` "im a cat burglar"
-- "hey" `isPrefixOf` "hey there!"
-- "there!" `isSuffixOf` "oh hey there"
-- partition (>3) [1,3,5,6,3,2,1,0,3,7]
-- span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
-- find (>4) [1,2,3,4,5,6]
-- 10 `elemIndex` [1,2,3,4,5,6]
-- ' ' `elemIndices` "Where are the spaces?"
-- findIndex (==4) [5,3,2,1,6,4]
-- findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
-- zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
-- lines "first line\nsecond line\nthird line"
-- unlines ["first line","second line","third line"]
-- words "hey these are the words in this\nsentence"
-- unwords ["hey","there","mate"]
-- nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
-- delete 'h' . delete 'h' $ "hey there ghang!"
-- [1..10] \\ [2,5,9]
-- [1..7] `union` [5..10]
-- [1..7] `intersect` [5..10]
-- insert 3 [1,2,4,3,2,1]
-- genericLength,genericTake,genericDrop,genericSplitAt,genericIndex...
-- let xs = [1..6] in sum xs / genericLength xs
-- foldl' and foldl1' are stricter versions of their respective lazy incarnations.
-- nubBy,deleteBy,unionBy,intersectBy... group <=> groupBy (==)
-- let values = [-4.3,-2.4,-1.2,0.4,2.3,5.9,10.5,29.1,5.3,-2.4,-14.5,2.9,2.3]
-- groupBy (\x y -> (x > 0) == (y > 0)) values

-- import Data.Function
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- groupBy ((==) `on` (> 0)) values

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- sortBy,insertBy,maximumBy
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- Ordering: LT, EQ, GT    sort <=> sortBy compare

-- let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
-- sortBy (compare `on` length) xs

-- Data.Char

-- isAlpha :: Char -> Bool
-- all isAlphaNum "eddy the fish!"
-- filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

-- generalCategory :: Char -> GeneralCategory
-- map generalCategory " \t\nA9?|"

-- map digitToInt "FF85AB"
-- intToDigit 15

-- map ord "abcdefgh"
-- map chr [97,98,99,100,101,102,103,104]

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
   in map chr shifted

-- encode 5 "Marry Christmas! Ho ho ho!"

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- decode 5 . encode 5 $ "This is a sentence"

phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- findKey key xs = snd . head . filter (\(k, _) -> key == k) $ xs
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey _ [] = Nothing
-- findKey key ((k, v) : xs) =
--   if key == k
--     then Just v
--     else findKey key xs

findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

-- findKey "penny" phoneBook

-- Map.fromList [(1,2),(3,4),(3,2),(5,5)]
-- Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
-- Map.null Map.empty
-- Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
-- Map.insert 5 9 $ Map.singleton 3 9
-- Map.lookup 2 [(2,3),(3,4)]
-- Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
-- Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
-- Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
-- Map.toList . Map.insert 9 2 $ Map.singleton 4 3

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

-- keys <=> map fst . Map.toList, elems <=> map snd . Map.toList

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2)

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

-- Map.lookup "betty" $ phoneBookToMap phoneBook
-- Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
-- Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
-- Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]

-- Data.Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"

text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

set1 = Set.fromList text1

set2 = Set.fromList text2

-- Set.intersection set1 set2
-- Set.difference set1 set2
-- Set.union set1 set2
-- Set.size $ Set.fromList [3,4,5,3,4,5]
-- Set.singleton 9
-- Set.insert 4 $ Set.fromList [9,3,8,1]
-- Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
-- Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
-- Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
-- Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
-- setNub xs = Set.toList $ Set.fromList xs