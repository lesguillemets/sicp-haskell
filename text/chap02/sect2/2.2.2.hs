module HierarchicalStructures where

data TList a = Nil | Cons (TList a) (TList a) | V a deriving (Show)
-- We should probably doing this with Cons [TList a] | V a
-- Note that his allows lists that does not terminate with Nil, like
-- Cons (V 2) (V 1)
list :: [TList a] -> TList a
list = foldr Cons Nil

fromList :: [a] -> TList a
fromList = list . map V

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
-- >>> countLeaves $ Cons (list . map V $ [1,2]) (list . map V $ [3,4])
-- 4

scaleTree :: (Num a) => TList a -> a -> TList a
scaleTree tree factor = case tree of
                            Nil -> Nil
                            (V x) -> V (x*factor)
                            (Cons  pre post)
                                -> Cons (scaleTree pre factor)
                                        (scaleTree post factor)
-- |
-- >>> let l = list [V 1, list [V 2, list [V 3, V 4], V 5] , list [V 6, V 7]]
-- >>> l
-- Cons (V 1) (Cons (Cons (V 2) (Cons (Cons (V 3) (Cons (V 4) Nil)) (Cons (V 5) Nil))) (Cons (Cons (V 6) (Cons (V 7) Nil)) Nil))
-- >>> scaleTree l 10
-- Cons (V 10) (Cons (Cons (V 20) (Cons (Cons (V 30) (Cons (V 40) Nil)) (Cons (V 50) Nil))) (Cons (Cons (V 60) (Cons (V 70) Nil)) Nil))
