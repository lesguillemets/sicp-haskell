module TwoEighteen where
import SchemeList

rev :: SchList a -> SchList a
rev Nil = Nil
rev ls = let iter xs acc = case xs of
                               Nil -> acc
                               _ -> iter (cdr xs) (Cons (car xs) acc)
                               in
                                   iter ls Nil
-- |
-- >>> rev $ scL [1..4]
-- Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))
