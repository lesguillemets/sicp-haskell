module InternalDefinitions where

sqrt' x = let
    goodEnough guess = abs (square guess - x) < threshold
    improve guess = average guess (x/guess)
    sqrtIter guess = if goodEnough guess then guess
                                         else sqrtIter (improve guess)
    in
        sqrtIter 1.0


square :: Num a => a -> a
square = (^2)
average :: Double -> Double -> Double
average = ((/ 2) . ) . (+)
threshold :: Double
threshold = 0.0000001

main = do
    print $ sqrt' 1
    print $ sqrt' 3
    print $ sqrt' 1023
