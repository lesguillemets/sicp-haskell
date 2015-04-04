module IntervalArithmetic where

-- Let's try with tuple, not data types.

type Interval = (Double, Double)
makeInterval :: Double -> Double -> Interval
makeInterval a b = if a < b then (a,b) else (b,a)

newtype Foo = Foo (Double,Double)

-- Ex 2.7.
lowerBnd :: Interval -> Double
lowerBnd = fst
upperBnd :: Interval -> Double
upperBnd = snd

addInterval :: Interval -> Interval -> Interval
addInterval i0 i1 = makeInterval
    (lowerBnd i0 + lowerBnd i1) (upperBnd i0 + upperBnd i1)

mulInterval :: Interval -> Interval -> Interval
mulInterval i0 i1 = let
    ps = [
         lowerBnd i0 * lowerBnd i1,
         lowerBnd i0 * upperBnd i1,
         upperBnd i0 * lowerBnd i1,
         upperBnd i0 * upperBnd i1
         ]
    in
        makeInterval (minimum ps) (maximum ps)
    
divInterval :: Interval -> Interval -> Interval
divInterval i0 i1 = mulInterval i0 $
    makeInterval (1 / upperBnd i1) (1 / lowerBnd i1)

-- Ex 2.8.
subInterval :: Interval -> Interval -> Interval
subInterval i0 i1 = makeInterval
        (lowerBnd i0 - upperBnd i1) (upperBnd i0 - lowerBnd i1)
