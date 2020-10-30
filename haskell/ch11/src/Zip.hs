{- Zip Lists -}

-- import Control.Applicative

-- instance Applicative ZipList where
--   pure x = ZipList (repeat x)
--   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
-- getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- (,,) <=> \x y z -> (x, y, z) . (,) <=> \x y -> (x, y)

-- zipWith4 .. zipWith7
