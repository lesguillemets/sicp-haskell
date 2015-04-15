module Picture where

type Base = Float

-- ex 2.46
data Vector = Vector {_x :: Base, _y :: Base} deriving (Eq, Show)

instance Num Vector where
    v0 + v1 = Vector (_x v0 + _x v1) (_y v0 + _y v1)
    negate (Vector x0 y0) = Vector (-x0) (-y0)
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger n = Vector (fromIntegral n) 0

(.*.) :: Base -> Vector -> Vector
(.*.) a (Vector x y) = Vector (a*x) (a*y)
infixl 7 .*.

-- ex 2.47
data Frame = Frame {
           _origin :: Vector,
           _edge1 :: Vector,
           _edge2 :: Vector
        } deriving (Show)

type Painter = Frame -> IO ()
type Segment = (Vector, Vector)
drawLine = undefined
frameCoordMap = undefined

segmentsToPainter :: [Segment] -> Painter
segmentsToPainter segments frame =
    mapM_ (\segment -> drawLine (frameCoordMap frame (fst segment))
                                (frameCoordMap frame (snd segment)))
                                segments
