module NewtonMethod where

-- | sqrt by Newton's method.
-- >>> sqrtIter 1 2
-- 1.4142156862745097
--
-- prop> abs ((sqrtIter n)^2 - n) < 0.01

sqrtIter :: Double -> Double -> Double
sqrtIter guess x = if goodEnough guess x then guess
                                         else sqrtIter (improv guess x) x

threshold :: Double
threshold = 0.00001

improv :: Double -> Double -> Double
improv guess x = average guess (x/guess)

average :: Double -> Double -> Double
average = ((/ 2) .) . (+)

goodEnough :: Double -> Double -> Bool
goodEnough guess x = abs (x - guess^2) < threshold

main = do
    print $ sqrtIter 1 2
    print $ sqrtIter 1 3
    print $ sqrtIter 1 320
