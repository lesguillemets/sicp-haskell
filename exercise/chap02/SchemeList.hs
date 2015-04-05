{-# LANGUAGE BangPatterns #-}
module SchemeList where
data SchList a = Nil | Cons a (SchList a) deriving (Show)
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

len :: SchList a -> Int
len items = let
    iter !acc its = case its of
                         Nil -> acc
                         _ -> iter (acc+1) (cdr its)
    in iter 0 items

append :: SchList a -> SchList a -> SchList a
append l0 l1 = case l0 of
                   Nil -> l1
                   _ -> Cons (car l0) (append (cdr l0) l1)
