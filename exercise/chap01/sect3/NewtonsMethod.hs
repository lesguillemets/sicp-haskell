module NewtonsMethod where

newtonsMethod :: (Double -> Double) -> Double -> Double
newtonsMethod g guess = fixP (newtonTransform g) guess

newtonTransform :: (Double -> Double) -> Double -> Double
newtonTransform g x = x - (g x / deriv g x)

deriv :: (Double -> Double) -> Double -> Double
deriv g = \x -> (g (x+dx) - g x) / dx where dx = 0.00001

averaageDamp :: (Num a, Fractional a) => (a -> a) -> a -> a
averaageDamp f x = average x (f x)

fixP :: (Double -> Double) -> Double -> Double
fixP = fixedPoint' torelance

fixedPoint' :: (Num a, Ord a) => a -> (a -> a) -> a -> a
fixedPoint' tore f guess =
        let next = f guess
            in
                if closerThan tore guess next
                    then next
                    else fixedPoint' tore f next

average :: (Num a, Fractional a) => a -> a -> a
average = ((/2) . ) . (+)

torelance :: Double
torelance = 0.0000001
closerThan :: (Num a, Ord a) => a -> a -> a -> Bool
closerThan tor v0 v1 = abs (v1 - v0) < tor
