module OneThirtyone where
-- the concept of \prod itself, iterative version.
-- well, kind of fold.
prod' :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
prod' term a next b =
        let iter l result = if l > b
                                then result
                                else iter (next l) (result * term l)
                                in
                                    iter a 1

-- | works nicely.
-- >>> map factorial [1..10] == take 10 factorials
-- True
factorial :: Int -> Int
factorial = prod' id 1 succ

-- | pi/4  = \prod (i-1)(i+1) / (i*i), i <- [3,5,7,...]
-- >>> import Text.Printf
-- >>> printf "%.3f" $ piWith 10000
-- 3.142
piWith :: Int -> Double
piWith n = let
    next = (+2)
    term i = fromIntegral (i*i-1) / fromIntegral (i*i)
    in
        4 * prod' term 3 next (3+n)

-- retursive version.
-- >>> map (prod id 1 succ) [1..10] == take 10 factorials
-- True
prod :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
prod term a next b = if a > b then 1
                              else term a * prod term (next a) next b

factorials :: [Int]
factorials = scanl1 (*) [1..]
