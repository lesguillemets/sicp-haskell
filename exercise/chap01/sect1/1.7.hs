module NewtonMethod where

-- | sqrt by Newton's method.
-- >>> sqrtIter 1 2
-- 1.4142156862745097
--
-- prop> abs ((sqrtIter n)^2 - n) < 0.01

sqrtIter :: Double -> Double -> Double
sqrtIter guess x = fst . head . dropWhile ( (> threshold) . abs . uncurry (-) )
    . changes . map fst . iterate (flip (,) x . uncurry improv) $ (guess,x)

changes :: [a] -> [(a,a)]
changes a = zip a (tail a)

threshold :: Double
threshold = 0.00001

improv :: Double -> Double -> Double
improv guess x = avarage guess (x/guess)

avarage :: Double -> Double -> Double
avarage = ((/ 2) .) . (+)

goodEnough :: Double -> Double -> Bool
goodEnough guess x = abs (x - guess^2) < threshold

main = do
    print $ sqrtIter 1 2
    print $ sqrtIter 1 3
    print $ sqrtIter 1 320
