module TwoFortyone where

-- assuming i < j < k
tuplesLessThan :: Int -> [(Int, Int, Int)]
tuplesLessThan n = [ (i,j,k) | i <- [1..n], j <- [i+1..n], k <- [j+1..n]]

tuplesLessThan' :: Int -> [(Int,Int,Int)]
tuplesLessThan' n =
        concatMap (
            \i -> concatMap (
                \j -> map ((,,) i j) [j+1..n]
                ) [i+1..n]
        ) [1..n]


f :: Int -> Int -> [(Int,Int,Int)]
f n s = filter (\(x,y,z) -> x+y+z == s) . tuplesLessThan' $ n
-- |
-- >>> f 12 12
-- [(1,2,9),(1,3,8),(1,4,7),(1,5,6),(2,3,7),(2,4,6),(3,4,5)]
