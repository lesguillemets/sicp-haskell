module HierarchicalStructures where

data TList a = Nil | Cons (TList a) (TList a) | V a deriving (Show)
-- We should probably doing this with Cons [TList a] | V a
-- Note that his allows lists that does not terminate with Nil, like
-- Cons (V 2) (V 1)
list :: [a] -> TList a
list = foldr (Cons . V) Nil

car :: TList a -> TList a
car (Cons p _) = p
cdr :: TList a -> TList a
cdr (Cons _ q) = q

unpack :: TList a -> a
unpack (V a) = a

countLeaves :: TList a -> Int
countLeaves Nil = 0
countLeaves (V _) = 1
countLeaves (Cons p q) = countLeaves p + countLeaves q

-- |
-- >>> countLeaves $ Cons (list [1,2]) (list [3,4])
-- 4
