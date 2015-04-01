module TwoTwo where
type Point = (Double, Double)
type Segment = (Point,Point)
mkSegment :: Point -> Point -> Segment
mkSegment p0 p1 = if xPoint p0 < xPoint p1 then (,) p0 p1
                                           else (,) p1 p0

-- I know this is ugly, but it accords the text.
midPointSegment :: Segment -> Point
midPointSegment seg = let
    p0 = startSegment seg
    p1 = endSegment seg
    in
        midPoint p0 p1
-- |
-- >>> let p0 = mkPoint 3 5
-- >>> let p1 = mkPoint 5 12
-- >>> midPointSegment $ mkSegment p0 p1
-- (4.0,8.5)

midPoint :: Point -> Point -> Point
midPoint p0 p1 = mkPoint ((xPoint p0 + xPoint p1)/2) ((yPoint p0 + yPoint p1)/2)

startSegment :: Segment -> Point
startSegment = fst
endSegment :: Segment -> Point
endSegment = snd

mkPoint :: Double -> Double -> Point
mkPoint = (,)
xPoint :: Point -> Double
xPoint = fst
yPoint :: Point -> Double
yPoint = snd
