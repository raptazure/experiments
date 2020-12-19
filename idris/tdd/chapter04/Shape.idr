data Shape = 
  ||| A triangle, with its base length and height
  Triangle Double Double
  | ||| A rectangle, with its length and height
  Rectangle Double Double
  | ||| A circle, with its radius
  Circle Double

area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x
