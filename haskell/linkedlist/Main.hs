module MyList where

data MyList a = Cons a (MyList a) 
                | MyNil deriving (Show, Eq)
{-
 A simple linked list module
  Some examples: 
	mylist = (Cons 10 (Cons 99 (Cons 11 (Cons 1 MyNil))))
	myHead myList                       # => 10
	myTail myList                       # => Cons 99 (Cons 11 (Cons 1 MyNil))
	myLength myList               # => 4
	myToList myList               # => [10,99,11,1]
	myFromList [10,99,11,1]       # => (Cons 10 (Cons 99 (Cons 11 (Cons 1 MyNil))))
	myIndex 2 myList              # => 11
	myMapList (\x -> x*x) myList  # => Cons 100 (Cons 9801 (Cons 121 (Cons 1 MyNil)))
	...etc..
-}

myHead :: MyList a -> a
myHead l = case l of
        Cons a _ -> a

myTail :: MyList a -> MyList a
myTail MyNil = MyNil
myTail l = case l of
        Cons _ a -> a

myIndex :: Int -> MyList a -> a
myIndex 0 xs = myHead xs
myIndex x xs = myHead (myIndexTail x xs)
    where 
        myIndexTail 0 xs = xs
        myIndexTail i xs = myIndexTail (i-1) (myTail xs)

myLength :: MyList a -> Int 
myLength MyNil = 0
myLength xs = 1 + (myLength (myTail xs))

myLast :: MyList a -> a
myLast (Cons a MyNil) = a
myLast l = myLast (myTail l)

myInsert :: a -> MyList a -> MyList a
myInsert x xs = Cons x xs

myConcat :: MyList a -> MyList a -> MyList a
myConcat (Cons a MyNil) bs = Cons a bs
myConcat as bs = myInsert (myHead as) (myConcat (myTail as) bs)

myAppend :: a -> MyList a -> MyList a
myAppend x (Cons a MyNil) = Cons a (Cons x MyNil)
myAppend x xs = myInsert (myHead xs) (myAppend x (myTail xs))

myToList :: MyList a -> [a]
myToList MyNil = []
myToList (Cons a l) = a:(myToList l)

myFromList :: [a] -> MyList a 
myFromList [] = MyNil
myFromList l = Cons (head l) (myFromList (tail l))

myMapList :: (t -> a) -> MyList t -> MyList a
myMapList f (Cons x MyNil) = Cons (f x) MyNil
myMapList f l = Cons (f (myHead l)) (myMapList f (myTail l))