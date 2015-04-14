module TwoForty where
uniquePairs :: Int -> [(Int,Int)]
uniquePairs n = concatMap (\i -> map ((,) i) [i+1..n]) [1..n]
-- |
-- >>> uniquePairs 5
-- [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]

primeSumPairs :: Int -> [(Int,Int)]
primeSumPairs = filter (isPrime . uncurry (+)) . uniquePairs
-- |
-- >>> primeSumPairs 5
-- [(1,2),(1,4),(2,3),(2,5),(3,4)]

isPrime :: Int -> Bool
isPrime n = 0 `notElem` [n `mod` i | i <- [2..n-1]]
