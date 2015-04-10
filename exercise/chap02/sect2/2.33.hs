module TwoThirythree where

map' :: (a -> b) -> [a] -> [b]
map' p = foldr (\ x y -> p x:y) []
-- \x y -> (:) . p
-- |
-- >>> map' (*2) [1..5]
-- [2,4,6,8,10]

append :: [a] -> [a] -> [a]
append seq0 seq1 = foldr (:) seq1 seq0
-- |
-- >>> [1,2] `append` [5,6]
-- [1,2,5,6]

len :: [a] -> Int
len = foldr (\ x y -> y+1) 0
-- const (1 +)
-- |
-- >>> len [1..10]
-- 10

