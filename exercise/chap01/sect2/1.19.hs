module OneNineteen where
-- |p+q  q | |a|
-- | q   p | |b|
-- A^2 = ( ((p+q)^2 + q^2, 2pq+q^2), (2pq+q^2, p^2+q^2))

-- | fibonacci, which runs in a logarithmic number of steps.
-- >>> map fib [1..10]
-- [1,1,2,3,5,8,13,21,34,55]
fib = fibIter 1 0 0 1

fibIter a b p q count
    | count == 0 = b
    | even count = fibIter a b (sq p + sq q) (2*p*q + sq q) (halve count)
    | otherwise = fibIter (b*q + a*q + a*p) (b*p+a*q) p q (count-1)

sq :: Num a => a -> a
sq = (^2)
halve :: Integral a => a -> a
halve = (`div` 2)
