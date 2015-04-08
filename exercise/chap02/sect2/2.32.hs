module TwoThirtytwo where

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let rest = subsets xs in
    rest ++ map (x:) rest
-- |
-- >>> subsets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
