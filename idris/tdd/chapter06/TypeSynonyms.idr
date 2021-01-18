import Data.Vect

tri : Vect 3 (Double, Double)
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

-- use an initial capital letter for functions that computes types
Position : Type
Position = (Double, Double)

tri' : Vect 3 Position
tri' = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)] 

Polygon : Nat -> Type
Polygon k = Vect k Position

tri'' : Polygon 3
tri'' = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)] 
