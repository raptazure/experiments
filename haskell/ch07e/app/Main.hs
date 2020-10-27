module Main where

import Geometry
import GeometryFolder.Cube as Cube

myIntToStr :: Float -> String
myIntToStr x
    | x < 3     = show x ++ " is less than three"
    | otherwise = show x ++ " is bigger than three"


main :: IO()
main = putStr $ myIntToStr $ Cube.volume 3