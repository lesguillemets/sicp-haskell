module TwoThirtynine where

rev :: [a] -> [a]
rev = foldr (\x acc -> acc ++ [x]) []
-- |
-- rev [1..10]
-- [10,9,8,7,6,5,4,3,2,1]

rev' :: [a] -> [a]
rev' = foldl (flip (:)) []
-- |
-- rev' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
