import Data.List

data Shape = Triangle Double Double
            | Rectangle Double Double
            | Circle Double

Eq Shape where
  (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
  (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
  (==) (Circle x) (Circle x') = x == x'
  (==) _ _ = False

area : Shape -> Double
area (Triangle x y) = x * y * 0.5
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

Ord Shape where
  compare x y = compare (area x) (area y)

showShape : Shape -> String
showShape (Triangle x y) = "Triangle" ++ " " ++ show x ++ " " ++ show y ++ " / "
showShape (Rectangle x y) = "Rectangle" ++ " " ++ show x ++ " " ++ show y ++ " / "
showShape (Circle x) = "Circle" ++ " " ++ show x ++ " / "

Show (List Shape) where
  show [] = ""
  show (x :: xs) = showShape x ++ show xs

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
              Rectangle 2 7]

main : IO ()
main = printLn (sort testShapes)
