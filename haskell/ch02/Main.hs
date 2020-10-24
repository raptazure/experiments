doubleSmallNumber x = if x > 100
                      then x
                      else  x*2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- [1,2,3,4] ++ [9,10,11,12]  
-- [9.4,33.2,96.2,11.2,23.25] !! 1 
-- 'A':" SMALL CAT"
-- head [5,4,3,2,1] 
-- tail [5,4,3,2,1] 
-- last [5,4,3,2,1]  
-- init [5,4,3,2,1]
-- take 3 [5,4,3,2,1]
-- drop 3 [8,4,2,1,5,6]
-- minimum [8,4,2,1,5,6] 
-- 4 `elem` [3,4,5,6] 
-- [2,4..20]
-- [20,19..1]
-- take 10 (cycle [1,2,3])
-- take 10 (repeat 5)
-- replicate 3 10
-- [x*2 | x <- [1..10], x*2 >= 12]
-- [x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- [x * y | x <- [2,5,10], y <- [8,10,11], x * y > 50]
-- let nouns = ["hobo","frog","pope"]
-- let adjectives = ["lazy","grouchy","scheming"]
-- [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
-- [ [ x | x <- xs, even x ] | xs <- xxs]

-- fst (8,11)
-- snd ("Wow", False)
-- zip [1 .. 5] ["one", "two", "three", "four", "five"]

-- let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]