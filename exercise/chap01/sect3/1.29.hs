module OneTwentynine where

-- the concept of summation itself.
sum' :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
sum' term a next b = if a > b then 0
                              else term a + sum' term (next a) next b


-- constraints to Double for now.
simpson :: (Double -> Double) -> Double -> Double -> Int -> Double
simpson f a b n = let
    n' = n*2
    h = (b-a) / fromIntegral n'
    next = (+2)
    term i = 4* f (a + i*h) + 2 * f (a + (i+1)*h)
    sumInter = sum' term 1 next (fromIntegral n'-2)
    in
        h * (sumInter + f b - f a) / 3
-- | Not sure this is what is wanted.
-- | Anyway, let's try. \int_0^1 x^3 dx = 1/4.
-- >>> import Text.Printf
-- >>> printf "%.3f" $ simpson cube 0 1 10000
-- 0.250

cube :: (Num a) => a -> a
cube a  = a * a * a
