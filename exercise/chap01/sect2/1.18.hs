module OneEighteen where

-- | expt :: Exponential
-- >>> expt 3 5
-- 243
-- >>> expt 2 24
-- 16777216
expt :: Int -> Int -> Int
expt = fastExptIter 1 where
    fastExptIter a b n
        | n == 0 = a
        | even n = fastExptIter a (b `mul` b) (n`div`2)
        | otherwise = fastExptIter (a `mul` b) b (n-1)

mul :: (Num a, Integral b) => a -> b -> a
mul = mulIter 0 where
    mulIter acc a n
        | n == 0 = acc
        | even n = mulIter acc (double a) (halve n)
        | otherwise = mulIter (acc + a) a (n-1)

double :: (Num a) => a -> a
double = (*2)
halve :: (Integral a) => a -> a
halve = (`div` 2)
