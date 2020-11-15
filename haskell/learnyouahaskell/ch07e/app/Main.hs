module Main where

-- import Geometry
import GeometryFolder.Cube as Cube (volume)

myFloatToStr :: Float -> String
myFloatToStr x
  | x < 3 = show x ++ " is less than three"
  | otherwise = show x ++ " is bigger than three"

main :: IO ()
main = putStr $ myFloatToStr $ Cube.volume 3