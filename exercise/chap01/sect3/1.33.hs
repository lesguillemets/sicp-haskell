module OneThirtythree where

-- takeWhile seems to be a better explanation.
twAccumulate' :: (Ord a)
    => (t -> t -> t) -> t -> (a -> t) -> (a -> Bool) -> (a -> a) -> a -> t
-- Phew! I think Monoids can come very handy here.
twAccumulate' combiner nullValue term cond next a
    = let iter acc x =
            if not (cond x) then acc
                            else iter (acc `combiner` term x) (next x)
        in
           iter nullValue a

--  the sum of the squares of the prime numbers in the interval a to
--  b (assuming that you have a prime? predicate already written)
sumSqPrim :: Int -> Int -> Int
sumSqPrim a b = twAccumulate' (+) 0 (\n -> if isPrime n then n*n else 0)
                                                        (<= b) succ a

-- | Test : fmmm. ..  sum . map (^2) $ [2,3,5,7,11,13,17,19]
-- >>> sumSqPrim 2 20
-- 1027

-- Or this? Is this what it's asking for?
-- This does not seem to be a "generalization"...
filteredAccmulate' :: (Ord a)
    => (t -> t -> t) -> t ->
            (a -> t) -> a -> (a -> a) -> a -> (a -> Bool) -> t
filteredAccmulate' combiner nullValue term a next b cond
    = let iter acc x y
            | x > y = acc
            | cond x = iter (acc `combiner` term x) (next x) y
            | otherwise = iter acc (next x) y
        in
           iter nullValue a b

-- | the sum of the squares of the prime numbers in the interval a to
-- | b (assuming that you have a prime? predicate already written)
-- >>> sumSqPrim' 2 20
-- 1027
sumSqPrim' :: Int -> Int -> Int
sumSqPrim' a b = filteredAccmulate' (+) 0 (^2) a succ b isPrime

-- the product of all the positive integers less than n that are relatively
-- prime to n
-- | n = 10 : 1*3*7*9 = 189
-- >>> prodRPs 10
-- 189
--
-- | n = 11 : 10!
-- >>> prodRPs 11
-- 3628800
prodRPs :: (Integral a) => a -> a
prodRPs n = filteredAccmulate' (*) 1 id 1 succ n ((== 1) . gcd n)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
    = all (\i -> n `mod` i /= 0) $ 2:[3,5..(floor.sqrt.fromIntegral) n]
