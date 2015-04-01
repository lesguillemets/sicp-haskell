module TwoThree where

data Point = Point {xPoint :: Double, yPoint :: Double} deriving (Show)

-- scheme does not seem to have the notion of type or classes for now,
-- how is polymorphism possible with that? (We can still "pack" functinos
-- width and height  to the Rectangle, though.) Or we don't need to have two
-- definitions coexist?
class Rectangle r where
    width :: r -> Double
    height :: r -> Double
    area :: r -> Double
    area x = width x * height x
    perimeter :: r -> Double
    perimeter x = 2* (width x + height x)


data Rect0 = Rect0 {upLeft0 :: Point, bottomRight0 ::  Point} deriving (Show)

instance Rectangle Rect0 where
    width r = (xPoint . bottomRight0) r - (xPoint . upLeft0) r
    height r = (yPoint . upLeft0) r - (yPoint . bottomRight0) r

-- |
-- >>> let r = Rect0 (Point 0 5) (Point 4 0)
-- >>> area r
-- 20.0
-- >>> perimeter r
-- 18.0

data Size = Size {sWidth :: Double, sHeight :: Double} deriving (Show)
data Rect1 = Rect1 {upLeft :: Point, size :: Size}

instance Rectangle Rect1 where
    width = sWidth . size
    height = sHeight . size

-- |
-- >>> let r = Rect1 (Point 0 5) (Size 4 5)
-- >>> area r
-- 20.0
-- >>> perimeter r
-- 18.0
