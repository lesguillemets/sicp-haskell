{-# LANGUAGE BangPatterns #-}
module OneFortysix where
-- $setup
-- >>> import Text.Printf

iterativeImprove :: (a -> Bool) -> (a -> a) -> a -> a
iterativeImprove test improve !guess =
        if test guess then guess
                      else iterativeImprove test improve (improve guess)

-- these constraints for Double come from `threshold`
sqrt' :: Double -> Double
sqrt' x = iterativeImprove closeEnough next 1.0
    where
        closeEnough = (< threshold) . absDiff x . square
        next guess = average guess (x/guess)
-- |
-- >>> printf "%.7f" $ sqrt' 3
-- 1.7320508

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess = iterativeImprove goodGuess f firstGuess
    where
        goodGuess x = absDiff x (f x) < threshold
-- |
-- >>> printf "%.5f" $ fixedPoint cos 1.0
-- 0.73908

threshold :: Double
threshold = 0.000001
square :: Num a => a -> a
square x = x*x
average :: Fractional a => a -> a -> a
average x y = (x+y)/2
absDiff :: Num a => a -> a -> a
absDiff = (abs .) . (-)
