module NewtonMethod where

sqrtIter :: Double -> Double -> Double
sqrtIter guess x = if goodEnough guess x then guess
                                         else sqrtIter (improv guess x) x

improv :: Double -> Double -> Double
improv guess x = avarage guess (x/guess)

avarage :: Double -> Double -> Double
avarage = ((/ 2) .) . (+)

goodEnough :: Double -> Double -> Bool
goodEnough guess x = abs (x - guess^2) < 0.0001

main = do
    print $ sqrtIter 1 2
