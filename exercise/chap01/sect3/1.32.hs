module OneThirtytwo where
-- the concept of \prod itself, iterative version.
-- well, kind of fold.

-- | more general form.
-- >>> sumAcc id 0 succ 10 == sum [0..10]
-- True
accumulate :: (Ord a)
    => (t -> t -> t) -> t -> (a -> t) -> a -> (a -> a) -> a -> t
accumulate combiner nullValue term a next b
    = if a > b then nullValue
               else term a `combiner`
                        accumulate combiner nullValue term (next a) next b
sumAcc :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumAcc = accumulate (+) 0

-- | Iterative version.
-- >>> sumAcc' id 0 succ 100 == sum [0..100]
-- True
accumulate' :: (Ord a)
    => (t -> t -> t) -> t -> (a -> t) -> a -> (a -> a) -> a -> t
accumulate' combiner nullValue term a next b
    = let iter acc x y =
            if x > y then acc
                     else iter (acc `combiner` term x) (next x) y
        in
           iter nullValue a b
sumAcc' :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumAcc' = accumulate' (+) 0
