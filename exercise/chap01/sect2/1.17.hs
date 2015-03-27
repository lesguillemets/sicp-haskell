module OneSeventeen where
-- The exponentiation algorithms in this section are based on performing
-- exponentiation by means of repeated multiplication.
-- In a similar way, one can perform integer multiplication by means of
-- repeated addition. The following multiplication procedure is analogous
-- to the expt procedure:
-- (define (* a b)
--   (if (= b 0)
--       0
--       (+ a (* a (- b 1))))
-- This algorithm takes a number of steps that is linear in b. Now supporse
-- we include, together with addition, operations `double`, which doubles
-- an integer, and `halve`, which divides an (even) integer by 2.
-- Using these, design a multiplication procedure analogous to fast-expt
-- that uses a logarithmic number of steps.


double :: (Num a) => a -> a
double = (*2)
halve :: (Integral a) => a -> a
halve = (`div` 2)

-- | multiplication
-- >>> mul 2 5
-- 10
-- >>> mul 432 2342
-- 1011744
mul :: (Num a, Integral b) => a -> b -> a
mul a n
    | n == 0 = 0
    | even n = mul (double a) (halve n)
    | otherwise = a + mul a (n-1)
