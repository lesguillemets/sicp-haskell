module OneThirty where

-- the concept of summation itself, iterative version.
sum' :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
sum' term a next b =
        let iter l result = if l > b
                                then result
                                else iter (next l) (result + term l)
                                in
                                    iter a 0

-- | Let's check.
-- >>> sum' id 1 succ 10
-- 55
-- >>> sum' (^3) 1 succ 10
-- 3025
-- >>> import Text.Printf
-- >>> let piTerm x = 1 / fromIntegral (x*(x+2))
-- >>> let myPi = 8* sum' piTerm 1 (+ 4) 100000 :: Double
-- >>> printf "%.3f" myPi
-- 3.142
