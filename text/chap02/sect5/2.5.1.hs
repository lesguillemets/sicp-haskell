{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- do we really need this?
module GenericArithmeticOperations where
import Prelude hiding (Rational)
import ComplexNumber

class Arithmetic a where
    add :: a -> a -> a
    sub :: a -> a -> a
    mul :: a -> a -> a
    divide :: a -> a -> a

newtype SchemeNumber = SNumber Int deriving (Show)
instance Arithmetic SchemeNumber where
    add (SNumber x) (SNumber y) = SNumber (x+y)
    sub (SNumber x) (SNumber y) = SNumber (x-y)
    mul (SNumber x) (SNumber y) = SNumber (x*y)
    divide (SNumber x) (SNumber y) = SNumber (x `div` y)

data Rational = Rat {numer :: Int, denom :: Int} deriving (Show)
makeRat :: Int -> Int -> Rational
makeRat n d = let g = gcd n d
                  in
                  Rat (n`div`g) (d`div`g)

instance Arithmetic Rational where
    add x y =
        makeRat (numer x * denom y + numer y * denom x) (denom x*denom y)
    sub x y =
        makeRat (numer x * denom y - numer y * denom x) (denom x*denom y)
    mul x y = makeRat (numer x * numer y) (denom x * denom y)
    divide x y = makeRat (numer x * denom y) (denom x * numer y)

-- needs UndecidableInstances extension.
-- see : http://stackoverflow.com/questions/3213490/
instance (Complex a) => Arithmetic a where
    add z0 z1 =
        makeFromRealImag
            (realPart z0 + realPart z1)
            (imagPart z0 + imagPart z1)
    sub z0 z1 =
        makeFromRealImag
            (realPart z0 - realPart z1)
            (imagPart z0 - imagPart z1)
    mul z0 z1 = makeFromMagAng
            (magnitude z0 * magnitude z1)
            (angle z0 + angle z1)
    divide z0 z1 = makeFromMagAng
            (magnitude z0 / magnitude z1)
            (angle z0 - angle z1)
-- This isn't good, we can't add ComplexPolar and ComplexRect.
