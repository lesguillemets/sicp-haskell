module FindFixedPoints where

torelance :: Double
torelance = 0.00001
closerThan :: (Num a, Ord a) => a -> a -> a -> Bool
closerThan tor v0 v1 = abs (v1 - v0) < tor

fixedPoint :: (Num a, Ord a) => a -> (a -> a) -> a -> a
fixedPoint tore f firstGuess =
        let closeEnough = closerThan tore
            try guess = let next = f guess in
                if closeEnough guess next then next else try next
        in
            try firstGuess

-- you can use normal recursion, too..
fixedPoint' :: (Num a, Ord a) => a -> (a -> a) -> a -> a
fixedPoint' tore f guess =
        let next = f guess
            in
                if closerThan tore guess next
                    then next
                    else fixedPoint' tore f next


fixedPointHere :: (Double -> Double) -> Double -> Double
fixedPointHere = fixedPoint torelance
-- | cos theta = theta
-- >>> import Text.Printf
-- >>> printf "%.5f" $ fixedPointHere cos 1.0
-- 0.73908
-- >>> printf "%.5f" $ fixedPointHere (\y -> sin y + cos y) 1.0
-- 1.25873

-- y s.t. y*y = x. The equivalent form is y = x/y, so
-- we're looking for a fixed point for \y -> x/y.
-- This does not converge, though.
_sqrt' :: Double -> Double
_sqrt' x = fixedPointHere (x/) 1.0

-- y = (y+ x/y) / 2
sqrt' :: Double -> Double
sqrt' x = fixedPointHere (\y -> (y + x/y)/2) 1.0
-- | This works!
-- >>> import Text.Printf
-- >>> printf "%.11f" $ sqrt' 2
-- 1.41421356237
