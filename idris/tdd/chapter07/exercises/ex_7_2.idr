data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

showOp : Show a => String -> a -> a -> String
showOp op x y = "(" ++ show x ++ op ++ show y ++ ")"

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = showOp " + " x y
    show (Sub x y) = showOp " - " x y
    show (Mul x y) = showOp " * " x y
    show (Div x y) = showOp " / " x y
    show (Abs x) = "abs " ++ show x

(Neg a, Integral a, Eq a, Abs a) => Eq (Expr a) where
    (==) x y = eval x == eval y

-- the Integer (cast x)
(Neg num, Integral num, Abs num) => Cast (Expr num) num where
    cast orig = eval orig

x : Expr Integer
x = 6 * 3 + 12

y : Expr Integer
y = 2 + 4
