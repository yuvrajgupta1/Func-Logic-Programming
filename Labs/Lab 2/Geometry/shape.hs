
data Point = Point Float Float deriving (Show)
data Shape where
  Circle :: Point -> Float -> Shape
  Rectangle :: Point -> Point -> Shape
  deriving Show

let
    area :: Shape ‐> Float
    area (Circle _ r) = pi * r ^ 2
    area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 ‐ x1) * (abs $ y2 ‐ y1)