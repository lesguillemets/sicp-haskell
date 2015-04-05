-- {-# LANGUAGE ExistentialQuantification #-}
-- data Schlist = Nil | forall a. Pair (a, Schlist) deriving (Show)
data SchList a = Nil | Cons a (SchList a) deriving (Show)
-- Hmm.. We'd like this to be somewhat more arbitrary.
scL :: [a] -> SchList a
scL [] = Nil
scL [x] = Cons x Nil
scL (x:xs) = Cons x (scL xs)

car :: SchList a -> a
car (Cons a _) = a
car Nil = error "car '()"

cdr :: SchList a -> SchList a
cdr (Cons _ xs) = xs

listRef :: SchList a -> Int -> a
listRef items 0 = car items
listRef items n = listRef (cdr items) (n-1)
-- |
-- >>> listRef (scL [i*i | i <- [1..20]]) 3
-- 16

len :: SchList a -> Int
len Nil = 0
len lst = 1 + len (cdr lst)
-- |
-- >>> len $ scL [1..5]
-- 5

len' :: SchList a -> Int
len' items = let
    iter acc its = case its of
                         Nil -> acc
                         _ -> iter (acc+1) (cdr its)
    in iter 0 items
-- |
-- >>> len' $ scL [1..5]
-- 5

append :: SchList a -> SchList a -> SchList a
append l0 l1 = case l0 of
                   Nil -> l1
                   _ -> Cons (car l0) (append (cdr l0) l1)
-- |
-- >>> append (scL [3,2,1]) (scL [10,20,30])
-- Cons 3 (Cons 2 (Cons 1 (Cons 10 (Cons 20 (Cons 30 Nil)))))
