module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L : ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R : ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

newTree = changeToP' [R, L] freeTree

des = elemAt [R, L] newTree

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r : bs) = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x l t, bs)

x -: f = f x

wcrTree :: (Tree Char, Breadcrumbs Char)
wcrTree = (freeTree, []) -: goRight -: goLeft

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

newFocus :: Zipper Char
newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')

newFocus2 :: Zipper Char
newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

farLeft :: (Tree Char, Breadcrumbs Char)
farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft

newFocus3 :: Zipper Char
newFocus3 = farLeft -: attach (Node 'Z' Empty Empty)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

resTree :: Zipper Char
resTree = topMost newFocus3

{- Focusing on Lists -}
data List a = Empty' | Cons a (List a) deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)

xs :: [Integer]
xs = [1, 2, 3, 4]

res1 :: ListZipper Integer
res1 = goForward (xs, [])

res2 :: ListZipper Integer
res2 = goForward ([2, 3, 4], [1])

res3 :: ListZipper Integer
res3 = goForward ([3, 4], [2, 1])

res4 :: ListZipper Integer
res4 = goBack ([4], [3, 2, 1])

-- If you were making a text editor, you could use a list of strings to rep- resent the lines of text that are currently opened,
-- and you could then use a zipper so that you know on which line the cursor is currently focused.
-- Using a zipper would also make it easier to insert new lines anywhere in the text or delete existing ones.

{- A Simple Filesystem -}
type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

newFocus4 = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"

newFocus5 = newFocus4 -: fsUp -: fsTo "watermelon_smash.gif"

answer :: IO ()
answer = do
  print $ fst newFocus5

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

newFocus6 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
  (Folder folderName (item : items), bs)

newFocus7 = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp

-- By using zippers, we get versioning for free.
-- We can always refer to older versions of data structures, even after we’ve changed them.
-- This isn’t unique to zippers, but it is a property of Haskell, because its data structures are immutable.
-- With zippers, however, we get the ability to easily and efficiently walk around our data structures, so the persistence of Haskell’s data struc- tures really begins to shine.

{- Watch Your Step -}
goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft' (Empty, _) = Nothing

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight' (Empty, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing

coolTree :: Tree Integer
coolTree = Node 1 Empty (Node 3 Empty Empty)

newFocus8 :: Maybe (Zipper Integer)
newFocus8 = return (coolTree, []) >>= goRight' >>= goRight'

newFocus9 :: Maybe (Zipper Integer)
newFocus9 = return (coolTree, []) >>= goRight' >>= goRight' >>= goRight'
