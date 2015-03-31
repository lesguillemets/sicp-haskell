module ProceduresAsReturnedValues where
import FindFixedPoints (fixedPoint')
-- $setup
-- >>> import Text.Printf

averaageDamp :: (Num a, Fractional a) => (a -> a) -> a -> a
averaageDamp f x = average x (f x)

fixP :: (Double -> Double) -> Double -> Double
fixP = fixedPoint' torelance

-- | sqrt n
-- >>> printf "%.6f" $ sqrt' 2
-- 1.414214
sqrt' :: Double -> Double
sqrt' x = fixP (averaageDamp (x /)) 1.0

-- | We can reuse the same expression
-- >>> printf "%.6f" $ cubeRoot 3
-- 1.442250
cubeRoot :: Double -> Double
cubeRoot x = fixP (averaageDamp ((x /) . square)) 1.0

-- Newton's method
-- f(x) = x - g(x)/g'(x) -- very clean expression for this method..

-- I want these function to be more generic (e.g. I want this function to
-- work for Double -> Complex, too), but it reduces the simplicity for the
-- examples. I'll stick with Double -> Double for now.
-- I'm moved by the beauty of partial applications...
deriv :: (Double -> Double) -> Double -> Double
deriv g = \x -> (g (x+dx) - g x) / dx where dx = 0.00001
-- | d(x^3)/dx = 3x^2.
-- >>> printf "%.5f" $ deriv (^3) 5
-- 75.00015
newtonTransform :: (Double -> Double) -> Double -> Double
newtonTransform g x = x - (g x / deriv g x)

newtonsMethod :: (Double -> Double) -> Double -> Double
newtonsMethod g guess = fixP (newtonTransform g) guess
-- | cos (pi/2) = 0
-- >>> printf "%.9f" . abs $ pi - 2*newtonsMethod cos 1.0
-- 0.000000000

-- We can use this to make another kind of sqrt:
sqrt'' :: Double -> Double
sqrt'' x = newtonsMethod (subtract x . square) 1.0
-- | sqrt 3
-- >>> printf "%.6f" $ sqrt'' 3
-- 1.732051

-- Abstractions and first-class procedures

fixedPointOfTransform :: (Double -> Double) ->
    ( (Double -> Double) -> Double -> Double) ->
        Double -> Double
fixedPointOfTransform g transf = fixP (transf g)

-- aaaand yet another sqrt
sqrt''' :: Double -> Double
sqrt''' x = fixedPointOfTransform (x /) averaageDamp 1.0
-- | sqrt 5
-- >>> printf "%.6f" $ sqrt''' 5
-- 2.236068

-- Or
sqrt'''' :: Double -> Double
sqrt'''' x = fixedPointOfTransform (subtract x . square) newtonTransform 1.0
-- | sqrt 5
-- >>> printf "%.5f" $ sqrt'''' 7
-- 2.64575

-- Helper functions

average :: (Num a, Fractional a) => a -> a -> a
average = ((/2) . ) . (+)

square :: (Num a) => a -> a
square x = x*x

torelance :: Double
torelance = 0.0000001
