module ProceduesAsAruments where

sumInts :: (Integral a) => a -> a -> a
sumInts a b = if a > b then 0 else a + sumInts (a+1) b

sumCubes :: (Integral a) => a -> a -> a
sumCubes a b = if a > b then 0 else cube a + sumInts (a+1) b

piSum :: (Integral a) => a -> a -> Double
piSum a b = if a > b then 0
                     else 1 / fromIntegral (a*(a+2)) + piSum (a+4) b

-- the concept of summation itself.
sum' :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
sum' term a next b = if a > b then 0
                              else term a + sum' term (next a) next b

sumInts' :: (Integral a) => a -> a -> a
sumInts' a b = sum' id a succ b
sumCubes' :: (Integral a) => a -> a -> a
sumCubes' a b = sum' cube a succ b
piSum' :: (Integral a) => a -> a -> Double
piSum' a b = let piTerm x = 1 / fromIntegral (x * (x+2))
                 piNext = (+ 4) in
                     sum' piTerm a piNext b
-- | Now use that abstraction:
-- >>> sumInts' 1 10
-- 55
-- >>> sumCubes' 1 10
-- 3025
-- >>> import Text.Printf
-- >>> printf "%.2f" $ 8 * piSum' 1 10000
-- 3.14

-- constraints to Double for now.
integral :: (Double -> Double) -> Double -> Double -> Double -> Double
integral f a b dx = dx * sum' f (a + dx/2) (+ dx) b
-- | Let's try. \int_0^1 x^3 dx = 1/4.
-- >>> import Text.Printf
-- >>> printf "%.6f" $ integral cube 0 1 0.001
-- 0.250000

cube :: (Num a) => a -> a
cube a  = a * a * a
