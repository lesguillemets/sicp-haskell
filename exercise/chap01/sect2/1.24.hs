import System.Random

import Timer

main = do
    g <- getStdGen
    let fP = fastPrime g 100
        searchPrimeF = searchPrimeWith fP 3 :: Integer -> [Integer]
    mapM (timeIt "done"  . searchPrimeF) (take 10 $ iterate (*10) 100)
        >>= mapM_ print

searchPrimeWith :: (Integral a) => (a -> Bool) -> Int -> a -> [a]
searchPrimeWith f n lowerBound = take n . filter f $ [lowerBound..]

fermatTest :: (Random a, Integral a) => a -> StdGen -> (Bool, StdGen)
fermatTest n gen =
        let tryIt a = expMod a n n == a
            (rand,nextGen) = randomR (1,n-1) gen
            in
                (tryIt rand, nextGen)

fastPrime :: (Random a, Integral a) => StdGen -> Int -> a -> Bool
fastPrime gen times n =
        all fst . take times . iterate (fermatTest n . snd) $ (True,gen)

expMod :: (Integral a) => a -> a -> a -> a
-- I believe this is not very haskell-ish.
expMod base ex m
    | ex == 0 = 1
    | even ex = square (expMod base (ex `div` 2) m) `mod` m
    | otherwise = (base * expMod base (ex -1) m) `mod` m
square :: (Num a) => a -> a
square = (^2)
