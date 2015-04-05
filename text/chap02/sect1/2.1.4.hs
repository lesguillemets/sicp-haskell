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

-- Ex 2.9.
width :: Interval -> Double
width i = (upperBnd i - lowerBnd i) / 2

-- (m0-w0, m0+w0) + (m1-w1, m1+w1) = (m-w, m+w) where
-- m := m0+m1, w := m0+m1
-- the width of (m0-w0, m0+w0) * (m1-w1, m1+w1) contains m0, m1 in them

-- Ex 2.10.
multiInterval' :: Interval -> Interval -> Maybe Interval
multiInterval' i0 i1 = if spanZero i0 || spanZero i1
                           then Nothing
                           else Just (mulInterval i0 i1)
spanZero :: Interval -> Bool
spanZero i = lowerBnd i * upperBnd i < 0

-- Ex 2.11.
data Sign = Negative | Positive | Spanning
sign :: Interval -> Sign
sign i = case (lowerBnd i `compare` 0, upperBnd i `compare` 0) of
             (GT,GT) -> Positive
             (LT,LT) -> Negative
             _  -> Spanning


mulInterval'' :: Interval -> Interval -> Interval
mulInterval'' i0@(l0,l1) i0'@(l0',l1') =
        case (sign i0, sign i0') of
            (Positive, Positive) -> makeInterval (l0*l0') (l1*l1')
            (Positive, Spanning) -> makeInterval (l1*l0') (l1*l1')
            (Positive, Negative) -> makeInterval (l1*l0') (l0*l1')
            (Spanning, Positive) -> makeInterval (l0*l1') (l1*l1')
            (Spanning, Spanning) -> makeInterval (min (l0*l1') (l1*l0'))
                                                 (max (l1*l1') (l0*l0'))
            (Spanning, Negative) -> makeInterval (l1*l0') (l0*l0')
            (Negative, Positive) -> makeInterval (l0*l1') (l1*l0')
            (Negative, Spanning) -> makeInterval (l0*l1') (l0*l0')
            (Negative, Negative) -> makeInterval (l1*l1') (l0*l0')

-- |
-- >>> let nums = [(2,3), (-4,3), (-10,-2)] :: [(Double,Double)]
-- >>> let takeDiff (p,q) (r,s) = (abs (p-r), abs (q-s))
-- >>> let f x y = print $ takeDiff (mulInterval x y) (mulInterval'' x y)
-- >>> mapM_ (uncurry f) [(x,y) | x <- nums, y <- nums]
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)
-- (0.0,0.0)

-- Ex 2.12
makeCenterWidth :: Double -> Double -> Interval
makeCenterWidth c w = makeInterval (c-w) (c+w)

center :: Interval -> Double
center i = (lowerBnd i + upperBnd i) / 2

-- percent/100
ratio :: Interval -> Double
ratio i = width i / center i
makeCenterRatio :: Double -> Double -> Interval
makeCenterRatio c r = makeCenterWidth c (c*r)

-- Ex 2.13
-- (1±r0) * (1±r1) = 1 ْْ± r0+r1 + r0r1 ~= 1 ± (r0+r1)

-- | Ex 2.14
-- >>> import Text.Printf
-- >>> let a = makeCenterRatio 2 0.01
-- >>> let b = makeCenterRatio 4 0.02
-- >>> printf "%.3f" $ ratio (a `divInterval` a)
-- 0.020
-- >>> printf "%.3f" $ ratio (a `divInterval` b)
-- 0.030
