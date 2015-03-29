import System.Random
import Control.Monad

-- | can find primes
-- >>> g <- getStdGen
-- >>> let nums = [2..10] :: [Int]
-- >>> let isP = fastPrime g 10 :: Int -> Bool
-- >>> print $ filter isP nums
-- [2,3,5,7]
-- >>> print $ filter isP [10000..10010]
-- [10007,10009]
--
-- | and doesn't get fooled by Carmichael numbers:
-- >>> g <- getStdGen
-- >>> let isP = fastPrime g 100
-- >>> isP 2821
-- False
-- >>> isP 9746347772161
-- False

millarRabinTest :: (Random a, Integral a) => a -> StdGen -> (Bool, StdGen)
millarRabinTest n gen =
        let (rand,nextGen) = randomR (1,n-1) gen
            result = expModSense rand (n-1) n == 1
            in
                (result, nextGen)

fastPrime :: (Random a, Integral a) => StdGen -> Int -> a -> Bool
fastPrime gen times n =
        all fst . take times . iterate (millarRabinTest n . snd) $ (True,gen)

expModSense :: (Integral a) => a -> a -> a -> a
-- I believe this is not very haskell-ish.
expModSense base ex m
    | ex == 0 = 1
    | even ex = case liftM (`mod` m) $ squareCheck (expModSense base (ex `div` 2) m)
                    of
                        Nothing -> 0
                        Just n -> n
    | otherwise = (base * expModSense base (ex -1) m) `mod` m
squareCheck :: (Num a, Eq a) => a -> Maybe a
squareCheck x = let s = x*x
                    in if x /= 1 && s == 1 then Nothing else Just s
