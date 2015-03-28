module PrimaltyTest where
import System.Random

-- | O(sqrt n).
-- >>> map (fromEnum . isPrime) [2..11]
-- [1,1,0,1,0,1,0,0,0,1]
isPrime :: (Integral a) => a -> Bool
isPrime n = n == smallestDivisor n
smallestDivisor :: (Integral a) => a -> a
smallestDivisor = flip findDivisor 2

findDivisor :: (Integral a) => a -> a -> a
findDivisor n testDiv
    | square testDiv > n = n
    | testDiv `doesDivide` n = testDiv
    | otherwise = findDivisor n (testDiv + 1)

square :: (Num a) => a -> a
square = (^2)
doesDivide :: (Integral a) => a -> a -> Bool
doesDivide = ((== 0) . ) . flip mod

-- | O(log n) primalty test.
-- >>> g <- getStdGen
-- >>> let nums = [2..11] :: [Int]
-- >>> map (fromEnum . fastPrime g 10) nums
-- [1,1,0,1,0,1,0,0,0,1]
-- >>> let largerSet = [1000 .. 10000]
-- >>> map (fastPrime g 100) nums == map isPrime nums
-- True
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
