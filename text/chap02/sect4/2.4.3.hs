module ComplexNumber where
-- Couldn't find out how to implement the table, because of the
-- possible inconsistency of the functions
-- (Map (TypeName, Operation) f doesn't work because we can't fix the
-- type for f.
--
-- Alternatively, the idea of looking up implementation from the type
-- and the operation required seems to be the basis of typeclass.
-- put can be associated with instance Complex type where op = item.
-- get can be associated with calling the function.

class Show a => Complexy a where
    realPart :: a -> Double
    imagPart :: a -> Double
    magnitude :: a -> Double
    angle :: a -> Double
    makeFromRealImag :: Double -> Double -> a
    makeFromMagAng :: Double -> Double -> a

data ComplexRect = CompR Double Double deriving (Show)

instance Complexy ComplexRect where
    realPart (CompR r _) = r
    imagPart (CompR _ i) = i
    magnitude z = sqrt $ square (realPart z) + square (imagPart z)
    angle z = atan (imagPart z / realPart z)
    makeFromRealImag = CompR
    makeFromMagAng r a = CompR ((*) r (cos a)) ((*) r (sin a))

data ComplexPolar = CompP Double Double deriving (Show)

instance Complexy ComplexPolar where
    magnitude (CompP r _) = r
    angle (CompP _ a) = a
    realPart z = (*) (magnitude z) (cos (angle z))
    imagPart z = (*) (magnitude z) (sin (angle z))
    makeFromRealImag x y = CompP (sqrt (square x + square y)) (atan (y/x))
    makeFromMagAng = CompP

square :: Num a => a -> a
square = (^(2::Int))

data Complex = Rectangular ComplexRect
             | Polar ComplexPolar
             deriving (Show)

-- FIXME
instance Complexy Complex where
    realPart (Rectangular z) = realPart z
    realPart (Polar z) = realPart z
    imagPart (Rectangular z) = imagPart z
    imagPart (Polar z) = imagPart z
    magnitude (Rectangular z) = magnitude z
    magnitude (Polar z) = magnitude z
    angle (Polar z) = angle z
    angle (Rectangular z) = angle z
    makeFromRealImag x y = Rectangular $ makeFromRealImag x y
    makeFromMagAng x y = Polar $ makeFromMagAng x y
