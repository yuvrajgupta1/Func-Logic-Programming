module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = 2 * ( rectArea a b + rectArea b c + rectArea a c )

rectArea :: Float -> Float -> Float
rectArea a b = a * b 
